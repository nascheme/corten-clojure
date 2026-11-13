//! Kernel input streams and console output.
//!
//! An input stream is an instance of the kernel type
//! `corten.kernel/Stream` with fields `[string pos line handle]`. A
//! string stream reads `string` (UTF-16 units) at `pos`; a stream on
//! standard input or a file has `handle`, an index into a Rust-side
//! table that owns the buffer. `pos` counts the units read and `line` the
//! newlines read. Handle 0 is standard input: the console stream
//! (`G::ConsoleIn`), which survives an image save and load.
//!
//! Output goes to the process's standard output and error, through
//! Rust's line-buffered `stdout`.

use crate::clj::{G, INSTANCE_FIELDS, global, is_instance, instance_type, make_instance, set_global};
use crate::error::lisp_error;
use crate::object::*;
use std::cell::RefCell;
use std::io::{BufRead, Write};

const S_STRING: usize = INSTANCE_FIELDS;
const S_POS: usize = INSTANCE_FIELDS + 1;
const S_LINE: usize = INSTANCE_FIELDS + 2;
const S_HANDLE: usize = INSTANCE_FIELDS + 3;

/// The field names of `corten.kernel/Stream`.
pub const STREAM_FIELD_NAMES: [&str; 4] = ["string", "pos", "line", "handle"];

enum Source {
    Stdin,
    File(std::io::BufReader<std::fs::File>),
}

struct Handle {
    src: Source,
    /// The current line, as UTF-16 units, and the read position in it.
    buf: Vec<u16>,
    pos: usize,
    eof: bool,
}

thread_local! {
    static HANDLES: RefCell<Vec<Option<Handle>>> = RefCell::new(vec![Some(stdin_handle())]);
}

fn stdin_handle() -> Handle {
    Handle { src: Source::Stdin, buf: Vec::new(), pos: 0, eof: false }
}

/// Decode one line of input: UTF-8 if valid, else Latin-1.
fn decode_line(bytes: &[u8]) -> Vec<u16> {
    match std::str::from_utf8(bytes) {
        Ok(s) => s.encode_utf16().collect(),
        Err(_) => bytes.iter().map(|&b| b as u16).collect(),
    }
}

fn add_handle(src: Source) -> usize {
    HANDLES.with(|h| {
        let mut h = h.borrow_mut();
        let handle = Some(Handle { src, buf: Vec::new(), pos: 0, eof: false });
        match h.iter().position(Option::is_none) {
            Some(i) => {
                h[i] = handle;
                i
            }
            None => {
                h.push(handle);
                h.len() - 1
            }
        }
    })
}

fn with_handle<T>(s: LispObj, f: impl FnOnce(&mut Handle) -> T) -> T {
    let h = s.uref(S_HANDLE).integer() as usize;
    HANDLES.with(|t| match t.borrow_mut().get_mut(h).and_then(|x| x.as_mut()) {
        Some(handle) => f(handle),
        None => lisp_error("Stream is closed"),
    })
}

fn make_stream(string: LispObj, handle: LispObj) -> LispObj {
    let zero = LispObj::fixnum(0);
    make_instance(global(G::StreamType), &[string, zero, zero, handle])
}

/// Create the console stream (`crate::clj::init`).
pub(crate) fn init() {
    set_global(G::ConsoleIn, make_stream(NIL, LispObj::fixnum(0)));
}

/// The console stream, reading standard input.
pub fn console() -> LispObj {
    global(G::ConsoleIn)
}

/// After loading an image: forget the handles of the previous process
/// (handle 0 is a fresh standard input).
pub fn reattach_console() {
    HANDLES.with(|h| *h.borrow_mut() = vec![Some(stdin_handle())]);
}

pub fn is_stream(x: LispObj) -> bool {
    is_instance(x) && instance_type(x) == global(G::StreamType)
}

pub fn check_stream(x: LispObj) -> LispObj {
    if !is_stream(x) {
        crate::error::type_error(format!("Not a stream: {}", crate::clj::printer::pr_str(x)));
    }
    x
}

/// A stream reading file `path`.
pub fn make_file_input_stream(path: &str) -> LispObj {
    let f = std::fs::File::open(path)
        .unwrap_or_else(|e| lisp_error(format!("Could not open file {path} for reading: {e}")));
    let h = add_handle(Source::File(std::io::BufReader::new(f)));
    make_stream(NIL, LispObj::fixnum(h as i64))
}

/// A stream reading the (simple) string `str`.
pub fn make_string_input_stream(str: LispObj) -> LispObj {
    make_stream(str, NIL)
}

/// Close `s`: a file's handle is released; a string stream is left as is.
pub fn close_stream(s: LispObj) {
    let h = s.uref(S_HANDLE);
    if h.is_fixnum() && h.integer() != 0 {
        HANDLES.with(|t| t.borrow_mut()[h.integer() as usize] = None);
        s.uset(S_HANDLE, LispObj::fixnum(-1));
    }
}

/// Read the next line of a handle into its buffer; false at the end.
fn fill(handle: &mut Handle) -> bool {
    if handle.eof {
        return false;
    }
    let mut bytes = Vec::new();
    let r = match &mut handle.src {
        Source::Stdin => {
            let _ = std::io::stdout().flush();
            std::io::stdin().lock().read_until(b'\n', &mut bytes)
        }
        Source::File(f) => f.read_until(b'\n', &mut bytes),
    };
    match r {
        Ok(0) => {
            handle.eof = true;
            false
        }
        Ok(_) => {
            if bytes.ends_with(b"\r\n") {
                bytes.truncate(bytes.len() - 2);
                bytes.push(b'\n');
            }
            handle.buf = decode_line(&bytes);
            handle.pos = 0;
            true
        }
        Err(e) => lisp_error(format!("Read error: {e}")),
    }
}

fn bump(s: LispObj, i: usize, d: i64) {
    s.uset(i, LispObj::fixnum(s.uref(i).integer() + d));
}

/// The number of units read from `s`.
pub fn position(s: LispObj) -> i64 {
    s.uref(S_POS).integer()
}

/// The number of newlines read from `s`.
pub fn line_number(s: LispObj) -> i64 {
    s.uref(S_LINE).integer()
}

/// The next UTF-16 unit of `s`, or None at the end.
pub fn get_character(s: LispObj) -> Option<u32> {
    let str = s.uref(S_STRING);
    let c = if str != NIL {
        let i = s.uref(S_POS).integer() as usize;
        if i >= str.vector_length().integer() as usize {
            return None;
        }
        unsafe { *str.char_array_start().add(i) }
    } else {
        with_handle(s, |h| {
            if h.pos == h.buf.len() && !fill(h) {
                return None;
            }
            h.pos += 1;
            Some(h.buf[h.pos - 1])
        })?
    };
    bump(s, S_POS, 1);
    if c == b'\n' as u16 {
        bump(s, S_LINE, 1);
    }
    Some(c as u32)
}

/// Put back the unit last read from `s` (a no-op at the start of a
/// handle's buffer).
pub fn unread_character(s: LispObj) {
    let c = if s.uref(S_STRING) != NIL {
        let i = s.uref(S_POS).integer() as usize;
        if i == 0 {
            return;
        }
        unsafe { *s.uref(S_STRING).char_array_start().add(i - 1) }
    } else {
        let c = with_handle(s, |h| {
            if h.pos == 0 {
                return None;
            }
            h.pos -= 1;
            Some(h.buf[h.pos])
        });
        match c {
            Some(c) => c,
            None => return,
        }
    };
    bump(s, S_POS, -1);
    if c == b'\n' as u16 {
        bump(s, S_LINE, -1);
    }
}

/// Discard the buffered input of `s` (the rest of a console line).
pub fn clear_input(s: LispObj) {
    let str = s.uref(S_STRING);
    if str != NIL {
        s.uset(S_POS, str.vector_length());
    } else {
        with_handle(s, |h| h.pos = h.buf.len());
    }
}

/// Write `text` to standard output.
pub fn write_out(text: &str) {
    let _ = std::io::stdout().lock().write_all(text.as_bytes());
}

/// Write `text` to standard error, after flushing standard output so
/// the two interleave in order.
pub fn write_err(text: &str) {
    flush_out();
    let mut err = std::io::stderr().lock();
    let _ = err.write_all(text.as_bytes());
    let _ = err.flush();
}

pub fn flush_out() {
    let _ = std::io::stdout().flush();
}
