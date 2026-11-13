//! Regular expressions for the JavaScript `RegExp` stand-in of the ported
//! cljs.core (clj/cljs/host.cljs), on the `fancy-regex` crate
//! (backtracking for lookaround and backreferences, as Java has; the
//! `regex` crate otherwise). Patterns are compiled once and cached by
//! source and flags. Indices are in UTF-16 code units, as Lisp strings
//! are. Java's `\Q...\E` quoting and `\h`/`\H` are translated.

use std::cell::RefCell;
use std::collections::HashMap;

use fancy_regex::Regex;

use crate::heap::string_node;
use crate::object::*;
use crate::runtime::without_gc;

thread_local! {
    static CACHE: RefCell<HashMap<(String, String), Regex>> = RefCell::new(HashMap::new());
}

/// Java's horizontal whitespace (`\h`), as class contents.
const HSPACE: &str = " \\t\\x{A0}\\x{1680}\\x{180E}\\x{2000}-\\x{200A}\\x{202F}\\x{205F}\\x{3000}";

/// Translate the Java syntax the crate lacks: `\Q...\E` (a quoted
/// literal) and `\h`/`\H`.
fn translate(source: &str) -> String {
    let cs: Vec<char> = source.chars().collect();
    let mut out = String::with_capacity(source.len());
    let mut in_class = 0usize;
    let mut i = 0;
    while i < cs.len() {
        let c = cs[i];
        if c == '\\' && i + 1 < cs.len() {
            let d = cs[i + 1];
            i += 2;
            match d {
                'Q' => {
                    while i < cs.len() && !(cs[i] == '\\' && cs.get(i + 1) == Some(&'E')) {
                        if "\\.+*?()|[]{}^$#&-~".contains(cs[i]) {
                            out.push('\\');
                        }
                        out.push(cs[i]);
                        i += 1;
                    }
                    i += 2;
                }
                'h' if in_class > 0 => out.push_str(HSPACE),
                'h' => out.push_str(&format!("[{HSPACE}]")),
                'H' if in_class == 0 => out.push_str(&format!("[^{HSPACE}]")),
                _ => {
                    out.push(c);
                    out.push(d);
                }
            }
            continue;
        }
        match c {
            '[' => in_class += 1,
            ']' if in_class > 0 => in_class -= 1,
            _ => {}
        }
        out.push(c);
        i += 1;
    }
    out
}

fn compiled<R>(source: &str, flags: &str, f: impl FnOnce(&Regex) -> R) -> R {
    let key = (source.to_string(), flags.to_string());
    // Errors are raised once the cache is no longer borrowed (lisp_error
    // does not unwind the borrow).
    let err = CACHE.with(|c| {
        let mut c = c.borrow_mut();
        if c.contains_key(&key) {
            return None;
        }
        let mut inline = String::new();
        for ch in flags.chars() {
            match ch {
                'i' | 'm' | 's' | 'u' | 'x' => inline.push(ch),
                'g' | 'y' => {}
                _ => return Some(format!("Invalid regular expression flag: {ch}")),
            }
        }
        let source = translate(source);
        let pat = if inline.is_empty() { source } else { format!("(?{inline}){source}") };
        match Regex::new(&pat) {
            Ok(re) => {
                c.insert(key.clone(), re);
                None
            }
            Err(e) => Some(format!("Invalid regular expression /{}/: {e}", key.0)),
        }
    });
    if let Some(msg) = err {
        crate::error::argument_error(msg);
    }
    CACHE.with(|c| f(&c.borrow()[&key]))
}

fn str_arg(x: LispObj, what: &str) -> String {
    if !x.is_string() {
        crate::error::type_error(format!("{what}: not a string"));
    }
    x.string_to_rust()
}

/// UTF-16 index -> byte offset in `s` (clamped to the end).
fn byte_offset(s: &str, utf16: usize) -> usize {
    let mut n = 0;
    for (b, ch) in s.char_indices() {
        if n >= utf16 {
            return b;
        }
        n += ch.len_utf16();
    }
    s.len()
}

fn utf16_len(s: &str) -> usize {
    s.chars().map(char::len_utf16).sum()
}

/// `(k/regex-exec source flags string start)`: NIL, or a vector of the
/// match index and an array of the match and its groups (NIL for groups
/// that did not take part), searching from UTF-16 index `start`.
pub fn regex_exec(a: &[LispObj]) -> LispObj {
    let source = str_arg(a[0], "regex source");
    let flags = if a[1] == NIL { String::new() } else { str_arg(a[1], "regex flags") };
    let s = str_arg(a[2], "regex input");
    let start = if a[3].is_fixnum() { a[3].integer().max(0) as usize } else { 0 };
    if start > utf16_len(&s) {
        return NIL;
    }
    let groups: Result<Option<(usize, Vec<Option<String>>)>, String> = compiled(&source, &flags, |re| {
        let caps = match re.captures_from_pos(&s, byte_offset(&s, start)) {
            Ok(Some(caps)) => caps,
            Ok(None) => return Ok(None),
            Err(e) => return Err(format!("Regular expression /{source}/: {e}")),
        };
        let m0 = caps.get(0).unwrap();
        let index = utf16_len(&s[..m0.start()]);
        Ok(Some((index, caps.iter().map(|m| m.map(|m| m.as_str().to_string())).collect())))
    });
    let groups = groups.unwrap_or_else(|e| crate::error::error_of_kind(crate::error::ErrorKind::State, e));
    let Some((index, groups)) = groups else { return NIL };
    without_gc(|| {
        let items: Vec<LispObj> = groups
            .iter()
            .map(|g| g.as_deref().map_or(NIL, string_node))
            .collect();
        let arr = super::array::from_slice(&items);
        super::coll::vector_from(&[LispObj::fixnum(index as i64), arr])
    })
}

/// `(k/regex-group-count source flags)`: the number of capturing
/// groups.
pub fn regex_group_count(a: &[LispObj]) -> LispObj {
    let source = str_arg(a[0], "regex source");
    let flags = if a[1] == NIL { String::new() } else { str_arg(a[1], "regex flags") };
    LispObj::fixnum(compiled(&source, &flags, |re| re.captures_len()) as i64 - 1)
}
