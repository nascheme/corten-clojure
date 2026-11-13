//! Clojure reader.
//!
//! Produces Clojure data: lists, vectors, maps and sets are the kernel
//! collection types (`coll.rs`; a map literal is a PersistentArrayMap
//! of any size, a set a PersistentHashSet over one), with their
//! metadata. Syntax-quote expands into calls of the kernel functions
//! `k/list`, `k/concat`, `k/vec`, `k/sq-map` and `k/sq-set`, so it works
//! before `cljs.core` exists.

use std::sync::RwLock;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::*;
use crate::error::lisp_error;
use crate::heap::{self, string_node};
use super::coll::{self, list_from};
use crate::object::*;
use crate::runtime::{lisp_call, without_gc};
use crate::streams::{self, get_character, unread_character};

static GENSYM_COUNTER: AtomicUsize = AtomicUsize::new(1000);

/// Next number for generated symbol names.
pub fn next_id() -> usize {
    GENSYM_COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Clojure special forms: never namespace-qualified by syntax-quote.
pub const SPECIAL_FORMS: &[&str] = &[
    "def", "loop*", "recur", "if", "case*", "let*", "letfn*", "do", "fn*", "quote", "var",
    "import*", ".", "set!", "deftype*", "reify*", "try", "throw", "monitor-enter",
    "monitor-exit", "catch", "finally", "new", "&", "fields*",
];

/// Reader conditional features: `:corten` and `:default`, as jank has
/// `:jank` (not the JVM `:clj` ones). The cljs test runs add `:cljs`
/// (`k/set-reader-features`).
static FEATURES: RwLock<Option<Vec<String>>> = RwLock::new(None);

fn has_feature(name: &str) -> bool {
    match &*FEATURES.read().unwrap() {
        None => matches!(name, "corten" | "default"),
        Some(f) => f.iter().any(|x| x == name),
    }
}

/// Set the reader conditional features (names without the colon).
pub fn set_features(names: Vec<String>) {
    *FEATURES.write().unwrap() = Some(names);
}

/// Marker returned for `#_`-discarded forms and conditionals that match
/// nothing; never escapes `read_form`'s callers that accept it.
enum R {
    Form(LispObj),
    /// Nothing read (comment, discard, empty conditional).
    Skip,
    /// A closing delimiter.
    Close(u32),
    /// `#?@` elements to splice into the enclosing list.
    Splice(Vec<LispObj>),
    Eof,
}

struct Reader {
    s: LispObj,
    /// `foo#` gensyms of the innermost syntax-quote.
    gensyms: Option<Vec<(String, LispObj)>>,
    /// `%` parameters of the innermost `#(...)`: index (0 = `%&`).
    fn_args: Option<Vec<(usize, LispObj)>>,
    /// Depth of reader conditional branches not taken: tags are not read.
    suppress: u32,
    /// Reading data (`read-string`, edn), not code: metadata is kept.
    data: bool,
    /// Reading edn: no reader macros beyond edn's.
    edn: bool,
}

/// How `read_raw_mode` reads: code, Clojure data or edn.
#[derive(Clone, Copy, PartialEq)]
pub enum ReadMode {
    Code,
    Data,
    Edn,
}

fn is_ws(c: u32) -> bool {
    matches!(c, 0x20 | 0x09 | 0x0a | 0x0d | 0x0c | 0x2c)
}

fn is_terminating(c: u32) -> bool {
    is_ws(c)
        || matches!(
            char::from_u32(c),
            Some('"' | ';' | '@' | '^' | '`' | '~' | '(' | ')' | '[' | ']' | '{' | '}' | '\\')
        )
}

fn clj_sym(name: &str) -> LispObj {
    symbol_from(None, name)
}

fn err(msg: impl Into<String>) -> ! {
    lisp_error(format!("Clojure reader: {}", msg.into()))
}

impl Reader {
    fn next(&mut self) -> Option<u32> {
        get_character(self.s)
    }

    fn unread(&mut self, _c: u32) {
        unread_character(self.s);
    }

    fn peek(&mut self) -> Option<u32> {
        let c = self.next()?;
        self.unread(c);
        Some(c)
    }

    fn skip_ws(&mut self) -> Option<u32> {
        loop {
            let c = self.next()?;
            if !is_ws(c) {
                return Some(c);
            }
        }
    }

    /// Read one form; `Skip` results are skipped.
    fn read_form(&mut self) -> R {
        loop {
            match self.read1() {
                R::Skip => continue,
                r => return r,
            }
        }
    }

    /// Read a form that must be present (error at EOF or a delimiter).
    fn read_required(&mut self, what: &str) -> LispObj {
        match self.read_form() {
            R::Form(x) => x,
            R::Eof => err(format!("EOF while reading {what}")),
            R::Close(c) => err(format!("Unmatched delimiter {}", char::from_u32(c).unwrap_or('?'))),
            R::Splice(_) => err("Reader conditional splicing not allowed here"),
            R::Skip => unreachable!(),
        }
    }

    fn read1(&mut self) -> R {
        let Some(c) = self.skip_ws() else { return R::Eof };
        let ch = char::from_u32(c).unwrap_or('\u{fffd}');
        // edn has no quote, deref or syntax-quote: these start symbols.
        if self.edn && matches!(ch, '\'' | '@' | '`' | '~') {
            let t = self.read_token(c);
            return R::Form(self.interpret_token(&t));
        }
        match ch {
            '(' => {
                let l = self.read_list(')');
                R::Form(list_from(&l))
            }
            '[' => {
                let l = self.read_list(']');
                R::Form(coll::vector_from(&l))
            }
            '{' => {
                let l = self.read_list('}');
                R::Form(map_literal(&l))
            }
            ')' | ']' | '}' => R::Close(c),
            '"' => R::Form(self.read_string()),
            ';' => {
                self.skip_line();
                R::Skip
            }
            '\'' => R::Form(self.wrap("quote", None)),
            '@' => R::Form(self.wrap("deref", Some("clojure.core"))),
            '`' => {
                let saved = self.gensyms.replace(Vec::new());
                let x = self.read_required("syntax-quote");
                let r = self.syntax_quote(x);
                self.gensyms = saved;
                R::Form(r)
            }
            '~' => {
                if self.peek() == Some('@' as u32) {
                    self.next();
                    R::Form(self.wrap("unquote-splicing", Some("clojure.core")))
                } else {
                    R::Form(self.wrap("unquote", Some("clojure.core")))
                }
            }
            '^' => {
                let m = self.read_required("metadata");
                let m = meta_map(m);
                let x = self.read_required("form after metadata");
                R::Form(attach_meta(x, m, self.data))
            }
            '\\' => R::Form(self.read_char()),
            '#' => self.read_dispatch(),
            '%' if self.fn_args.is_some() => {
                let t = self.read_token(c);
                R::Form(self.fn_arg(&t))
            }
            _ => {
                let t = self.read_token(c);
                R::Form(self.interpret_token(&t))
            }
        }
    }

    fn wrap(&mut self, name: &str, ns: Option<&str>) -> LispObj {
        let x = self.read_required(name);
        list_from(&[symbol_from(ns, name), x])
    }

    /// Skip a comment: to the end of the line (`\n` or `\r`, as Clojure).
    fn skip_line(&mut self) {
        while let Some(c) = self.next() {
            if c == '\n' as u32 || c == '\r' as u32 {
                break;
            }
        }
    }

    /// Read forms up to `close`.
    fn read_list(&mut self, close: char) -> Vec<LispObj> {
        let mut v = Vec::new();
        loop {
            match self.read_form() {
                R::Form(x) => v.push(x),
                R::Splice(l) => v.extend(l),
                R::Close(c) if c == close as u32 => return v,
                R::Close(c) => err(format!(
                    "Unmatched delimiter: {}",
                    char::from_u32(c).unwrap_or('?')
                )),
                R::Eof => err(format!("EOF while reading, expected {close}")),
                R::Skip => unreachable!(),
            }
        }
    }

    fn read_token(&mut self, first: u32) -> String {
        let mut t = String::new();
        t.push(char::from_u32(first).unwrap_or('\u{fffd}'));
        while let Some(c) = self.next() {
            if is_terminating(c) {
                self.unread(c);
                break;
            }
            t.push(char::from_u32(c).unwrap_or('\u{fffd}'));
        }
        t
    }

    fn read_string(&mut self) -> LispObj {
        let mut units: Vec<u16> = Vec::new();
        loop {
            let Some(c) = self.next() else { err("EOF while reading string") };
            match c {
                0x22 => break,
                0x5c => {
                    let Some(e) = self.next() else { err("EOF while reading string") };
                    let v = match char::from_u32(e) {
                        Some('n') => 10,
                        Some('t') => 9,
                        Some('r') => 13,
                        Some('b') => 8,
                        Some('f') => 12,
                        Some('"') => 0x22,
                        Some('\\') => 0x5c,
                        Some('u') => self.read_hex(4),
                        Some(d @ '0'..='7') => {
                            let mut v = d as u32 - '0' as u32;
                            for _ in 0..2 {
                                match self.peek() {
                                    Some(d) if (0x30..=0x37).contains(&d) => {
                                        self.next();
                                        v = v * 8 + (d - 0x30);
                                    }
                                    _ => break,
                                }
                            }
                            v
                        }
                        _ => err(format!("Unsupported escape character: \\{}", char::from_u32(e).unwrap_or('?'))),
                    };
                    units.push(v as u16);
                }
                _ => units.push(c as u16),
            }
        }
        heap::string_from_units(&units)
    }

    fn read_hex(&mut self, n: usize) -> u32 {
        let mut v = 0;
        for _ in 0..n {
            let d = self.next().and_then(char::from_u32).and_then(|c| c.to_digit(16));
            let Some(d) = d else { err("Invalid unicode escape") };
            v = v * 16 + d;
        }
        v
    }

    fn read_char(&mut self) -> LispObj {
        let Some(c) = self.next() else { err("EOF while reading character") };
        let t = self.read_token(c);
        let mut chars = t.chars();
        let first = chars.next().unwrap();
        if chars.next().is_none() {
            return LispObj::character(first as u32);
        }
        let code = match t.as_str() {
            "newline" => 10,
            "space" => 32,
            "tab" => 9,
            "backspace" => 8,
            "formfeed" => 12,
            "return" => 13,
            _ if first == 'u' && t.len() == 5 => u32::from_str_radix(&t[1..], 16)
                .unwrap_or_else(|_| err(format!("Invalid character: \\{t}"))),
            _ if first == 'o' && t.len() <= 4 => match u32::from_str_radix(&t[1..], 8) {
                Ok(n) if n <= 0o377 => n,
                Ok(_) => err("Octal escape sequence must be in range [0, 377]"),
                Err(_) => err(format!("Invalid character: \\{t}")),
            },
            _ => err(format!("Unsupported character: \\{t}")),
        };
        LispObj::character(code)
    }

    fn read_dispatch(&mut self) -> R {
        let Some(c) = self.next() else { err("EOF after #") };
        let ch = char::from_u32(c).unwrap_or('\u{fffd}');
        if self.edn && !(matches!(ch, '{' | '_' | '^' | '#' | ':') || ch.is_ascii_alphabetic()) {
            err(format!("No dispatch macro for: #{ch}"));
        }
        match ch {
            '{' => {
                let l = self.read_list('}');
                if let Some(d) = coll::find_duplicate(&l) {
                    duplicate_key("Set", d);
                }
                R::Form(coll::hash_set_from(&l))
            }
            '(' => R::Form(self.read_fn()),
            '\'' => R::Form(self.wrap("var", None)),
            '"' => {
                let s = self.read_regex();
                // A new js/RegExp (clj/cljs/host.cljs).
                R::Form(list_from(&[kernel_fn("regexp"), s]))
            }
            '_' => {
                self.read_required("discarded form");
                R::Skip
            }
            '!' => {
                self.skip_line();
                R::Skip
            }
            '^' => {
                let m = self.read_required("metadata");
                let m = meta_map(m);
                let x = self.read_required("form after metadata");
                R::Form(attach_meta(x, m, self.data))
            }
            '#' => {
                let t = self.read_required("symbolic value");
                let v = if is_sym_named(t, "Inf") {
                    f64::INFINITY
                } else if is_sym_named(t, "-Inf") {
                    f64::NEG_INFINITY
                } else if is_sym_named(t, "NaN") {
                    f64::NAN
                } else if is_symbol(t) {
                    err(format!("Unknown symbolic value: {}", printer::pr_str(t)))
                } else {
                    err(format!("Invalid token: ##{}", printer::pr_str(t)))
                };
                R::Form(heap::double_float_node(v))
            }
            '?' => self.read_conditional(),
            ch if ch.is_ascii_alphabetic() => {
                let t = self.read_token(c);
                let x = self.read_required("form after a tag");
                if self.suppress > 0 {
                    return R::Form(NIL);
                }
                let f = var_value(global(G::ReadTagVar));
                if f != NIL {
                    return R::Form(lisp_call(f, &[string_node(&t), x]));
                }
                R::Form(tagged_literal(&t, x))
            }
            ':' => R::Form(self.read_namespaced_map()),
            _ => err(format!("No dispatch macro for: #{}", char::from_u32(c).unwrap_or('?'))),
        }
    }

    /// `#:ns{..}`, `#::{..}`, `#::alias{..}`: keys that are keywords or
    /// symbols without namespace get `ns`; those of namespace `_` lose it.
    fn read_namespaced_map(&mut self) -> LispObj {
        let eof = || err("EOF while reading namespaced map");
        let mut c = self.next().unwrap_or_else(eof);
        let auto = c == ':' as u32;
        if auto && self.edn {
            err("Namespaced map must specify a valid namespace");
        }
        if auto {
            c = self.next().unwrap_or_else(eof);
        }
        let name = if c == '{' as u32 || is_ws(c) {
            self.unread(c);
            String::new()
        } else {
            self.read_token(c)
        };
        if self.skip_ws() != Some('{' as u32) {
            err("Namespaced map must specify a map");
        }
        let ns = match (auto, name.is_empty()) {
            (true, true) => ns_name(current_ns()),
            (true, false) => ns_name(
                resolve_ns(current_ns(), string_node(&name))
                    .unwrap_or_else(|| err(format!("Unknown auto-resolved namespace alias: {name}"))),
            ),
            (false, false) => string_node(&name),
            (false, true) => err("Namespaced map must specify a namespace"),
        };
        let mut items = self.read_list('}');
        if !items.len().is_multiple_of(2) {
            err("Map literal must contain an even number of forms");
        }
        for i in (0..items.len()).step_by(2) {
            let key = items[i];
            if is_keyword(key) || is_symbol(key) {
                let kns = sym_ns(key);
                let new_ns = if kns == NIL {
                    Some(ns)
                } else if lisp_str_is(kns, "_") {
                    Some(NIL)
                } else {
                    None
                };
                if let Some(n) = new_ns {
                    let key = if is_keyword(key) {
                        keyword(n, sym_name(key))
                    } else {
                        let s = symbol(n, sym_name(key));
                        let m = sym_meta(key);
                        if m == NIL { s } else { sym_with_meta(s, m) }
                    };
                    items[i] = key;
                }
            }
        }
        map_literal(&items)
    }

    fn read_regex(&mut self) -> LispObj {
        let mut units: Vec<u16> = Vec::new();
        loop {
            let Some(c) = self.next() else { err("EOF while reading regex") };
            if c == 0x22 {
                break;
            }
            units.push(c as u16);
            if c == 0x5c {
                let Some(e) = self.next() else { err("EOF while reading regex") };
                units.push(e as u16);
            }
        }
        heap::string_from_units(&units)
    }

    /// `#?(...)` and `#?@(...)`.
    fn read_conditional(&mut self) -> R {
        let splice = self.peek() == Some('@' as u32);
        if splice {
            self.next();
        }
        // Whitespace may come before the list, as in Clojure.
        if self.skip_ws() != Some('(' as u32) {
            err("Reader conditional body must be a list");
        }
        // As in Clojure, branches that are not taken are read with tagged
        // literals unresolved, so a dialect's own tags (#cpp) do not fail.
        let mut result = R::Skip;
        loop {
            let k = match self.read_form() {
                R::Form(k) => k,
                R::Close(c) if c == ')' as u32 => break,
                R::Close(c) => err(format!("Unmatched delimiter: {}", char::from_u32(c).unwrap_or('?'))),
                R::Eof => err("EOF while reading reader conditional"),
                R::Splice(_) => err("Feature should be a keyword"),
                R::Skip => unreachable!(),
            };
            if !is_keyword(k) {
                err("Feature should be a keyword");
            }
            let name = k.uref(KW_NS) == NIL
                && has_feature(&k.uref(KW_NAME).string_to_rust());
            let take = name && matches!(result, R::Skip);
            if !take {
                self.suppress += 1;
            }
            let v = self.read_form();
            if !take {
                self.suppress -= 1;
            }
            let v = match v {
                R::Form(v) => v,
                R::Close(_) | R::Eof => err("Reader conditional requires an even number of forms"),
                R::Splice(_) if !take => continue,
                R::Splice(_) => err("Reader conditional splicing not allowed here"),
                R::Skip => unreachable!(),
            };
            if take {
                result = if splice {
                    if !coll::is_seq(v) && !coll::is_vector(v) {
                        err("Spliced form list in read-cond-splicing must implement java.util.List");
                    }
                    R::Splice(coll::items(v))
                } else {
                    R::Form(v)
                };
            }
        }
        result
    }

    /// `#(...)`: `(fn* [%1 ... & %&] (...))`.
    fn read_fn(&mut self) -> LispObj {
        if self.fn_args.is_some() {
            err("Nested #()s are not allowed");
        }
        self.fn_args = Some(Vec::new());
        let body = self.read_list(')');
        let args = self.fn_args.take().unwrap();
        let max = args.iter().map(|&(i, _)| i).max().unwrap_or(0);
        let mut params = Vec::new();
        for i in 1..=max {
            let s = match args.iter().find(|&&(j, _)| j == i) {
                Some(&(_, s)) => s,
                None => gen_param(i),
            };
            params.push(s);
        }
        if let Some(&(_, rest)) = args.iter().find(|&&(j, _)| j == 0) {
            params.push(clj_sym("&"));
            params.push(rest);
        }
        let pv = coll::vector_from(&params);
        list_from(&[clj_sym("fn*"), pv, list_from(&body)])
    }

    fn fn_arg(&mut self, t: &str) -> LispObj {
        let idx = match &t[1..] {
            "" => 1,
            "&" => 0,
            n => match n.parse::<usize>() {
                Ok(i) => i,
                Err(_) => return self.interpret_token(t),
            },
        };
        let args = self.fn_args.as_mut().unwrap();
        if let Some(&(_, s)) = args.iter().find(|&&(j, _)| j == idx) {
            return s;
        }
        let s = if idx == 0 {
            clj_sym(&format!("rest__{}#", next_id()))
        } else {
            gen_param(idx)
        };
        self.fn_args.as_mut().unwrap().push((idx, s));
        s
    }

    fn interpret_token(&mut self, t: &str) -> LispObj {
        match t {
            "nil" => return NIL,
            "true" => return TRUE,
            "false" => return FALSE,
            _ => {}
        }
        let b = t.as_bytes();
        if b[0].is_ascii_digit() || (b.len() > 1 && matches!(b[0], b'+' | b'-') && b[1].is_ascii_digit()) {
            return parse_number(t).unwrap_or_else(|| err(format!("Invalid number: {t}")));
        }
        if let Some(k) = t.strip_prefix(':') {
            return self.keyword_token(k, t);
        }
        let (ns, name) = split_name(t).unwrap_or_else(|| err(format!("Invalid token: {t}")));
        symbol_from(ns, name)
    }

    fn keyword_token(&mut self, k: &str, t: &str) -> LispObj {
        if let Some(k) = k.strip_prefix(':') {
            if self.edn {
                err(format!("Invalid token: {t}"));
            }
            // ::name or ::alias/name
            let (ns, name) = split_name(k).unwrap_or_else(|| err(format!("Invalid token: {t}")));
            let cur = current_ns();
            let target = match ns {
                None => cur,
                Some(a) => resolve_ns(cur, string_node(a))
                    .unwrap_or_else(|| err(format!("Invalid token: {t}"))),
            };
            let nsname = ns_name(target);
            return keyword(nsname, string_node(name));
        }
        if k.is_empty() || k.ends_with(':') {
            err(format!("Invalid token: {t}"));
        }
        let (ns, name) = split_name(k).unwrap_or_else(|| err(format!("Invalid token: {t}")));
        keyword_from(ns, name)
    }

    // -----------------------------------------------------------------------
    // Syntax-quote

    fn syntax_quote(&mut self, x: LispObj) -> LispObj {
        let quote = |x| list_from(&[clj_sym("quote"), x]);
        if is_symbol(x) {
            return quote(self.qualify(x));
        }
        if coll::is_empty_list(x) {
            return quote(x);
        }
        if coll::is_seq(x) {
            let items = coll::items(x);
            let head = items[0];
            if is_core_named(head, "unquote") {
                return coll::nth(x, 1);
            }
            if is_core_named(head, "unquote-splicing") {
                err("unquote-splicing used outside of a list");
            }
            return self.sq_concat(&items);
        }
        if coll::is_vector(x) {
            let l = self.sq_concat(&coll::items(x));
            return list_from(&[kernel_fn("vec"), l]);
        }
        if coll::is_map(x) || coll::is_set(x) {
            let (name, items) = if coll::is_map(x) {
                ("sq-map", coll::items(x).into_iter().flat_map(|e| [coll::nth(e, 0), coll::nth(e, 1)]).collect())
            } else {
                ("sq-set", coll::items(x))
            };
            let l = self.sq_concat(&items);
            return list_from(&[kernel_fn(name), l]);
        }
        if is_keyword(x) || x == NIL || x.is_string() || x.is_character() || is_boolean(x) {
            return x;
        }
        if crate::numbers::is_number(x) {
            return x;
        }
        quote(x)
    }

    /// `(k/concat (k/list a) b ...)` for `items`.
    fn sq_concat(&mut self, items: &[LispObj]) -> LispObj {
        let mut parts = vec![kernel_fn("concat")];
        for &item in items {
            let spliced = coll::is_seq(item)
                && !coll::is_empty_list(item)
                && is_core_named(coll::first(item), "unquote-splicing");
            parts.push(if spliced {
                coll::nth(item, 1)
            } else {
                let e = self.syntax_quote(item);
                list_from(&[kernel_fn("list"), e])
            });
        }
        list_from(&parts)
    }

    /// Namespace-qualify symbol `s` for syntax-quote.
    fn qualify(&mut self, s: LispObj) -> LispObj {
        let name = sym_name(s).string_to_rust();
        let ns = sym_ns(s);
        if ns == NIL && SPECIAL_FORMS.contains(&name.as_str()) {
            return s;
        }
        if ns == NIL
            && let Some(base) = name.strip_suffix('#')
        {
            let gs = self.gensyms.as_mut().unwrap();
            if let Some(&(_, g)) = gs.iter().find(|(n, _)| *n == name) {
                return g;
            }
            let g = clj_sym(&format!("{base}__{}__auto__", next_id()));
            self.gensyms.as_mut().unwrap().push((name, g));
            return g;
        }
        if ns != NIL {
            // An alias: use the full namespace name.
            return match resolve_ns(current_ns(), ns) {
                Some(p) => symbol(public_ns_name(p), sym_name(s)),
                None => s,
            };
        }
        if name.ends_with('.') || (name.contains('.') && name != "..") {
            // Class-like names, constructor calls and `.method` /
            // `.-field` interop are left alone.
            return s;
        }
        if let Some(m) = resolve_macro(s) {
            return symbol(public_ns_name(var_ns(m)), sym_name(s));
        }
        let home = resolve(s).map_or_else(current_ns, var_ns);
        symbol(public_ns_name(home), sym_name(s))
    }
}

fn gen_param(i: usize) -> LispObj {
    clj_sym(&format!("p{i}__{}#", next_id()))
}

/// Split `ns/name`; `/` alone and `ns//` are names.
fn split_name(t: &str) -> Option<(Option<&str>, &str)> {
    if t == "/" {
        return Some((None, t));
    }
    match t.find('/') {
        Some(0) => None,
        Some(i) => {
            let (ns, name) = (&t[..i], &t[i + 1..]);
            if name.is_empty() || (name.contains('/') && name != "/") {
                None
            } else {
                Some((Some(ns), name))
            }
        }
        None => Some((None, t)),
    }
}

fn is_core_named(x: LispObj, name: &str) -> bool {
    is_symbol(x)
        && lisp_str_is(sym_name(x), name)
        && (sym_ns(x) == NIL || lisp_str_is(sym_ns(x), "clojure.core"))
}

/// The symbol `corten.kernel/name`.
pub fn kernel_fn(name: &str) -> LispObj {
    symbol_from(Some("corten.kernel"), name)
}

/// A Long if it fits, else a BigInt (also with the `N` suffix).
fn parse_int(digits: &str, radix: u32, neg: bool, big: bool, t: &str) -> LispObj {
    match crate::numbers::parse_integer(digits, radix, neg, big) {
        Some(v) => v,
        None => err(format!("Invalid number: {t}")),
    }
}

/// Clojure number syntax: integers (with 0x, octal, radix `NrDDD`, `N`
/// suffix), ratios and floats (with `M` suffix).
pub fn parse_number(t: &str) -> Option<LispObj> {
    let (neg, body) = match t.as_bytes()[0] {
        b'-' => (true, &t[1..]),
        b'+' => (false, &t[1..]),
        _ => (false, t),
    };
    let big = body.ends_with('N');
    let body_n = body.strip_suffix('N').unwrap_or(body);
    if !body_n.is_empty() && body_n.bytes().all(|c| c.is_ascii_digit()) {
        if body_n.len() > 1 && body_n.starts_with('0') {
            return Some(parse_int(&body_n[1..], 8, neg, big, t));
        }
        return Some(parse_int(body_n, 10, neg, big, t));
    }
    if let Some(h) = body_n.strip_prefix("0x").or_else(|| body_n.strip_prefix("0X")) {
        return h.bytes().all(|c| c.is_ascii_hexdigit()).then(|| parse_int(h, 16, neg, big, t));
    }
    if let Some(i) = body.find(['r', 'R']) {
        let radix: u32 = body[..i].parse().ok()?;
        if !(2..=36).contains(&radix) {
            return None;
        }
        let d = &body[i + 1..];
        return d.chars().all(|c| c.is_digit(radix)).then(|| parse_int(d, radix, neg, false, t));
    }
    if let Some(i) = body.find('/') {
        let (n, d) = (&body[..i], &body[i + 1..]);
        if n.bytes().all(|c| c.is_ascii_digit()) && d.bytes().all(|c| c.is_ascii_digit()) && !d.is_empty() {
            return crate::numbers::parse_ratio(n, d, neg);
        }
        return None;
    }
    let f = body.strip_suffix('M').unwrap_or(body);
    let ok = f.bytes().all(|c| c.is_ascii_digit() || matches!(c, b'.' | b'e' | b'E' | b'+' | b'-'));
    if !ok {
        return None;
    }
    let v: f64 = f.parse().ok()?;
    Some(heap::double_float_node(if neg { -v } else { v }))
}

/// Elements of CL list `l` in a new simple-vector.
/// `#js [..]` is `(k/array ..)`; `#js {..}` is `(k/js-obj k v ..)`
/// with keyword keys given as their name strings (JavaScript objects,
/// see src/clj/js.rs).
/// The form for tagged literal `#tag x`: `#js`, and as in the
/// ClojureScript compiler, `#queue`, `#uuid` and `#inst` (constructed
/// when the form is evaluated).
fn tagged_literal(tag: &str, x: LispObj) -> LispObj {
    let core = |n: &str| symbol_from(Some("clojure.core"), n);
    match tag {
        "js" => js_literal(x),
        "queue" => {
            if !coll::is_pvector(x) {
                err("#queue requires a vector");
            }
            list_from(&[core("into"), symbol_from(Some("clojure.core"), "PersistentQueue.EMPTY"), x])
        }
        "uuid" => {
            if !x.is_string() {
                err("#uuid requires a string");
            }
            list_from(&[core("uuid"), x])
        }
        "inst" => {
            if !x.is_string() {
                err("#inst requires a string");
            }
            let ms = parse_timestamp(&x.string_to_rust())
                .unwrap_or_else(|| err(format!("Invalid #inst: {}", x.string_to_rust())));
            list_from(&[symbol_from(Some("js"), "Date."), LispObj::fixnum(ms)])
        }
        _ => err(format!("No reader function for tag #{tag}")),
    }
}

/// Milliseconds since the Unix epoch for an RFC 3339 timestamp
/// `yyyy[-mm[-ddThh[:mm[:ss[.fff]]]]][Z|(+|-)hh:mm]`.
pub fn parse_timestamp(t: &str) -> Option<i64> {
    let b = t.as_bytes();
    let mut i = 0;
    let num = |i: &mut usize, n: usize| -> Option<i64> {
        let s = t.get(*i..*i + n)?;
        if !s.bytes().all(|c| c.is_ascii_digit()) {
            return None;
        }
        *i += n;
        s.parse().ok()
    };
    let year = num(&mut i, 4)?;
    let (mut month, mut day, mut h, mut m, mut sec, mut ms) = (1, 1, 0, 0, 0, 0);
    if b.get(i) == Some(&b'-') {
        i += 1;
        month = num(&mut i, 2)?;
        if b.get(i) == Some(&b'-') {
            i += 1;
            day = num(&mut i, 2)?;
            if b.get(i) == Some(&b'T') {
                i += 1;
                h = num(&mut i, 2)?;
                if b.get(i) == Some(&b':') {
                    i += 1;
                    m = num(&mut i, 2)?;
                    if b.get(i) == Some(&b':') {
                        i += 1;
                        sec = num(&mut i, 2)?;
                        if b.get(i) == Some(&b'.') {
                            i += 1;
                            let start = i;
                            while b.get(i).is_some_and(|c| c.is_ascii_digit()) {
                                i += 1;
                            }
                            let frac = &t[start..i];
                            let digits: String = frac.chars().chain("000".chars()).take(3).collect();
                            ms = digits.parse().ok()?;
                        }
                    }
                }
            }
        }
    }
    let mut offset = 0;
    match b.get(i) {
        None => {}
        Some(b'Z') => i += 1,
        Some(&c) if c == b'+' || c == b'-' => {
            i += 1;
            let oh = num(&mut i, 2)?;
            if b.get(i) != Some(&b':') {
                return None;
            }
            i += 1;
            let om = num(&mut i, 2)?;
            offset = (oh * 60 + om) * if c == b'-' { -1 } else { 1 };
        }
        _ => return None,
    }
    if i != b.len() || !(1..=12).contains(&month) || !(1..=31).contains(&day) || h > 23 || m > 59 || sec > 60 {
        return None;
    }
    // Days from civil (Howard Hinnant's algorithm).
    let y = if month <= 2 { year - 1 } else { year };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (month + 9) % 12;
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146097 + doe - 719468;
    Some((((days * 24 + h) * 60 + m - offset) * 60 + sec) * 1000 + ms)
}

fn js_literal(x: LispObj) -> LispObj {
    if coll::is_pvector(x) {
        let mut items = vec![kernel_fn("array")];
        items.extend(coll::items(x));
        list_from(&items)
    } else if coll::is_array_map(x) {
        let mut items = vec![kernel_fn("js-obj")];
        for e in coll::items(x) {
            let k = coll::nth(e, 0);
            items.push(if is_keyword(k) { k.uref(KW_NAME) } else { k });
            items.push(coll::nth(e, 1));
        }
        list_from(&items)
    } else {
        err("#js requires a vector or a map")
    }
}

fn duplicate_key(what: &str, k: LispObj) -> ! {
    crate::error::argument_error(format!("{what} literal contains duplicate key: {}", printer::pr_str(k)))
}

/// The map read from `{...}` with items `kvs`.
fn map_literal(kvs: &[LispObj]) -> LispObj {
    if !kvs.len().is_multiple_of(2) {
        err("Map literal must contain an even number of forms");
    }
    let keys: Vec<LispObj> = kvs.iter().step_by(2).copied().collect();
    if let Some(d) = coll::find_duplicate(&keys) {
        duplicate_key("Map", d);
    }
    coll::array_map_from(kvs)
}

/// Metadata shorthand: `^:k` is `{:k true}`, `^sym` / `^"str"` is
/// `{:tag sym}`.
fn meta_map(m: LispObj) -> LispObj {
    if is_keyword(m) {
        coll::array_map_from(&[m, TRUE])
    } else if is_symbol(m) || m.is_string() {
        coll::array_map_from(&[keyword_from(None, "tag"), m])
    } else if coll::is_array_map(m) {
        m
    } else {
        err("Metadata must be Symbol, Keyword, String or Map")
    }
}

/// `m` merged into map `old` (or nil).
fn merge_meta(old: LispObj, m: LispObj) -> LispObj {
    if old == NIL {
        return m;
    }
    coll::items(m).into_iter().fold(old, |acc, e| coll::assoc(acc, coll::nth(e, 0), coll::nth(e, 1)))
}

/// Attach metadata `m` to `x` (merged into what it has). Values that
/// cannot carry metadata are an error when reading data; in code the
/// metadata (a type hint) is dropped.
fn attach_meta(x: LispObj, m: LispObj, data: bool) -> LispObj {
    let imeta = is_symbol(x)
        || coll::is_list(x)
        || coll::is_empty_list(x)
        || coll::is_pvector(x)
        || coll::is_array_map(x)
        || coll::is_hash_set(x);
    if !imeta {
        // Reading data (read-string, cljs.reader).
        if data || var_value(global(G::ReadTagVar)) != NIL {
            err("Metadata can only be applied to IMetas");
        }
        return x;
    }
    coll::with_meta(x, merge_meta(coll::meta(x), m))
}

thread_local! {
    static FORM_LINE: std::cell::Cell<i64> = const { std::cell::Cell::new(0) };
    static FORM_START: std::cell::Cell<i64> = const { std::cell::Cell::new(0) };
}

/// The line (from 1) where the form `read` last read starts.
pub fn form_line() -> i64 {
    FORM_LINE.with(|l| l.get())
}

/// Read one form of code; `None` at end of input.
pub fn read(stream: LispObj) -> Option<LispObj> {
    read_mode(stream, ReadMode::Code)
}

/// Read one form as `mode`; `None` at end of input.
pub fn read_mode(stream: LispObj, mode: ReadMode) -> Option<LispObj> {
    without_gc(|| {
        let mut r = Reader {
            s: stream,
            gensyms: None,
            fn_args: None,
            suppress: 0,
            data: mode != ReadMode::Code,
            edn: mode == ReadMode::Edn,
        };
        // Skip blanks and comments to find the line the form starts on.
        while let Some(c) = r.skip_ws() {
            if c == ';' as u32 {
                r.skip_line();
            } else {
                r.unread(c);
                break;
            }
        }
        FORM_LINE.with(|l| l.set(streams::line_number(stream) + 1));
        FORM_START.with(|l| l.set(streams::position(stream)));
        match r.read_form() {
            R::Form(x) => Some(x),
            R::Eof => None,
            R::Close(c) => err(format!("Unmatched delimiter: {}", char::from_u32(c).unwrap_or('?'))),
            R::Splice(_) => err("Reader conditional splicing not allowed at top level"),
            R::Skip => unreachable!(),
        }
    })
}

/// `(k/read stream &optional eof-error-p eof-value mode)`: mode nil
/// reads code, :data Clojure data (all metadata), :edn edn.
pub fn read_prim(a: &[LispObj]) -> LispObj {
    let mode = match a.get(3) {
        Some(&m) if m == keyword_from(None, "edn") => ReadMode::Edn,
        Some(&m) if m != NIL => ReadMode::Data,
        _ => ReadMode::Code,
    };
    let s = if a[0] == NIL || a[0] == TRUE { streams::console() } else { streams::check_stream(a[0]) };
    match read_mode(s, mode) {
        Some(x) => x,
        None if a.len() < 2 || truthy(a[1]) => err("EOF while reading"),
        None => a.get(2).copied().unwrap_or(NIL),
    }
}

/// `(k/form-source path line)`: the text of the first form at or
/// after line `line` of file `path`, or nil if it cannot be read.
pub fn form_source(a: &[LispObj]) -> LispObj {
    if !a[0].is_string() || !a[1].is_fixnum() {
        return NIL;
    }
    let Ok(text) = std::fs::read_to_string(a[0].string_to_rust()) else { return NIL };
    let skip = (a[1].integer() - 1).max(0) as usize;
    let rest: String = text.split_inclusive('\n').skip(skip).collect();
    let chars: Vec<char> = rest.chars().collect();
    let stream = streams::make_string_input_stream(string_node(&rest));
    match crate::error::catch_lisp_error(|| read(stream).is_some()) {
        Ok(true) => {
            let start = FORM_START.with(|l| l.get()).max(0) as usize;
            let end = (streams::position(stream).max(0) as usize).min(chars.len());
            string_node(&chars[start.min(end)..end].iter().collect::<String>())
        }
        _ => NIL,
    }
}

/// `(k/read-string string &optional edn)`: the first form in
/// `string`, read as data (or edn).
pub fn read_string_prim(a: &[LispObj]) -> LispObj {
    let s = streams::make_string_input_stream(a[0]);
    let mode = if a.len() > 1 && truthy(a[1]) { ReadMode::Edn } else { ReadMode::Data };
    read_mode(s, mode).unwrap_or_else(|| err("EOF while reading"))
}

// ---------------------------------------------------------------------------
// Runtime support for syntax-quote

/// `(k/sq-map kvs)`: the map of seq `kvs` (k v k v ...); later keys
/// replace earlier ones.
pub fn sq_map_prim(a: &[LispObj]) -> LispObj {
    coll::array_map_prim(&coll::items(a[0]))
}

/// `(k/sq-set xs)`: the set of the elements of seq `xs`.
pub fn sq_set_prim(a: &[LispObj]) -> LispObj {
    coll::hash_set_prim(&coll::items(a[0]))
}
