//! Command line entry point.
//!
//! `corten [--image IMG] [--disasm] [--boot FILE] [--load FILE]
//! [-e FORMS]... [--repl] [--save IMG]`
//! starts from heap image IMG (default: none, an empty kernel world),
//! loads the Clojure files and evaluates the Clojure FORMS (printing the
//! last value) in order, and starts the REPL if nothing else was given
//! (or with `--repl`). `--boot` loads a file with its directory as the
//! current directory, keeping the namespace it ends in (`clj/init.clj`
//! builds the Clojure system); `--save IMG` writes an image. The REPL is
//! `clojure.core/repl` if it is defined, else a kernel REPL.

use corten::compiler::{self, codegen::DISASSEMBLE};
use corten::error::catch_lisp_error;
use corten::prims::funcall;
use corten::{heap, image, init_lisp, init_lisp_from_image, streams};

fn usage() -> ! {
    eprintln!(
        "usage: corten [--image IMG] [--disasm] [--boot FILE] [--load FILE] [-e FORMS]... \
         [--repl] [--save IMG]"
    );
    std::process::exit(2);
}

fn report(e: corten::error::LispError) {
    flush_stdout();
    eprintln!("Error: {e}");
}

/// Evaluate every Clojure form in `text`, printing the last value.
fn eval_string(text: &str) -> bool {
    let r = catch_lisp_error(|| {
        let v = compiler::clj_eval_string(text);
        streams::write_out(&format!("{}\n", corten::clj::printer::pr_str(v)));
    });
    match r {
        Ok(()) => true,
        Err(e) => {
            report(e);
            false
        }
    }
}

fn load_file(path: &str) -> bool {
    let r = catch_lisp_error(|| compiler::clj_load(heap::string_node(path)));
    flush_stdout();
    if let Err(e) = r {
        report(e);
        return false;
    }
    true
}

/// The REPL: `clojure.core/repl` if it is defined, else a kernel REPL
/// (kernel reader, evaluator and printer).
fn repl() {
    use corten::clj;
    let repl = clj::find_ns("clojure.core")
        .and_then(|ns| clj::ns_lookup(ns, heap::string_node("repl")))
        .filter(|v| clj::is_bound(*v))
        .map(clj::var_value)
        .filter(|f| f.is_function());
    if let Some(f) = repl {
        if let Err(e) = catch_lisp_error(|| funcall(f, &[])) {
            report(e);
        }
        flush_stdout();
        return;
    }
    let input = streams::console();
    loop {
        let ns = clj::ns_name(clj::current_ns()).string_to_rust();
        streams::write_out(&format!("{ns}=> "));
        streams::flush_out();
        let r = catch_lisp_error(|| match clj::reader::read(input) {
            None => false,
            Some(x) => {
                let v = compiler::clj_eval(x);
                streams::write_out(&format!("{}\n", clj::printer::pr_str(v)));
                true
            }
        });
        match r {
            Ok(true) => {}
            Ok(false) => {
                streams::write_out("\n");
                streams::flush_out();
                break;
            }
            Err(e) => {
                report(e);
                streams::clear_input(input);
            }
        }
    }
}

/// Load a boot file with its directory as the current directory, so it
/// can refer to files next to it. The previous directory is restored.
/// `*ns*` is not rebound (`compiler::clj_boot`).
fn boot_file(path: &str) -> bool {
    let p = std::path::Path::new(path);
    let saved = std::env::current_dir().ok();
    let dir = p.parent().filter(|d| !d.as_os_str().is_empty());
    let name = p.file_name().map_or(path.to_string(), |n| n.to_string_lossy().into_owned());
    if let Some(d) = dir
        && let Err(e) = std::env::set_current_dir(d)
    {
        eprintln!("Cannot change directory to {}: {e}", d.display());
        return false;
    }
    let r = catch_lisp_error(|| compiler::clj_boot(heap::string_node(&name)));
    if let (Some(_), Some(s)) = (dir, saved) {
        let _ = std::env::set_current_dir(s);
    }
    flush_stdout();
    if let Err(e) = r {
        eprintln!("Error while loading {path}: {e}");
        return false;
    }
    true
}

fn flush_stdout() {
    streams::flush_out();
}

fn main() {
    let mut args: Vec<String> = std::env::args().skip(1).collect();
    if args.first().map(String::as_str) == Some("--image") {
        if args.len() < 2 {
            usage();
        }
        let path = args[1].clone();
        args.drain(..2);
        if let Err(e) = init_lisp_from_image(&path) {
            eprintln!("Cannot load image: {e}");
            std::process::exit(1);
        }
    } else {
        init_lisp();
    }
    let mut ok = true;
    let mut did_something = false;
    let mut want_repl = false;
    let mut it = args.iter();
    while let Some(a) = it.next() {
        match a.as_str() {
            "-e" => {
                let Some(text) = it.next() else { usage() };
                ok &= eval_string(text);
                did_something = true;
            }
            "--boot" => {
                let Some(path) = it.next() else { usage() };
                ok &= boot_file(path);
                did_something = true;
            }
            "--load" => {
                let Some(path) = it.next() else { usage() };
                ok &= load_file(path);
                did_something = true;
            }
            "--disasm" => DISASSEMBLE.store(true, std::sync::atomic::Ordering::Relaxed),
            "--repl" => want_repl = true,
            "--save" => {
                let Some(path) = it.next() else { usage() };
                let r = catch_lisp_error(|| image::save_image(path));
                if let Err(e) = r {
                    report(e);
                    ok = false;
                }
                did_something = true;
            }
            _ => usage(),
        }
    }
    if !did_something || want_repl {
        repl();
    }
    flush_stdout();
    std::process::exit(if ok { 0 } else { 1 });
}
