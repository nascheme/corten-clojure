//! Clojure reader and kernel printer (no Lisp system needed).

use corten::clj::{self, printer::pr_str, reader};
use corten::{heap, streams, with_lisp};

/// Read every form of `src` and print them, space separated.
fn rt(src: &str) -> String {
    with_lisp(|| {
        let s = streams::make_string_input_stream(heap::string_node(src));
        let mut out = Vec::new();
        while let Some(x) = reader::read(s) {
            out.push(pr_str(x));
        }
        out.join(" ")
    })
}

fn rt_err(src: &str) -> String {
    with_lisp(|| {
        let r = corten::error::catch_lisp_error(|| {
            let s = streams::make_string_input_stream(heap::string_node(src));
            reader::read(s);
        });
        match r {
            Ok(()) => "no error".to_string(),
            Err(e) => e.to_string(),
        }
    })
}

#[test]
fn atoms() {
    assert_eq!(rt("nil true false 1 -2 +3 1.5 1e3 2.0M 7N 1/2 4/2"), "nil true false 1 -2 3 1.5 1000.0 2.0 7N 1/2 2");
    assert_eq!(rt("9223372036854775807 9223372036854775808 -0x8000000000000000"), "9223372036854775807 9223372036854775808N -9223372036854775808");
    assert_eq!(rt("0x1F 017 2r1010 36rZ"), "31 15 10 35");
    let u = format!("{}u0041", '\\');
    let src = format!(r#""a\nb\"{u}" \a \space \newline {u}"#);
    assert_eq!(rt(&src), r#""a\nb\"A" \a \space \newline \A"#);
    assert_eq!(rt("##Inf ##-Inf ##NaN"), "##Inf ##-Inf ##NaN");
}

#[test]
fn symbols_and_keywords() {
    assert_eq!(rt("foo a.b/c / clojure.core// :k :a/b"), "foo a.b/c / clojure.core// :k :a/b");
    assert_eq!(rt("::k"), ":clojure.core/k");
    with_lisp(|| {
        let a = clj::keyword_from(Some("x"), "y");
        let b = clj::keyword_from(Some("x"), "y");
        assert_eq!(a, b, "keywords are interned");
        assert_ne!(clj::symbol_from(None, "s"), clj::symbol_from(None, "s"));
    });
}

#[test]
fn collections() {
    assert_eq!(rt("(1 (2) []) [a, b] {:a 1 :b 2} #{1} ()"), "(1 (2) []) [a b] {:a 1, :b 2} #{1} ()");
    assert!(rt_err("{1}").contains("even number"));
    assert!(rt_err("{:a 1 :a 2}").contains("duplicate key"));
    assert!(rt_err("#{[1] (1)}").contains("duplicate key"));
    assert_eq!(rt("^:m [1] ^{:a 1} (x)"), "[1] (x)");
    assert!(rt_err("(1 2").contains("EOF"));
    assert!(rt_err("(1 2]").contains("Unmatched"));
}

#[test]
fn dispatch_and_macros() {
    assert_eq!(rt("'a @b #'c #_ d e ; comment\n f"), "(quote a) (clojure.core/deref b) (var c) e f");
    assert_eq!(rt("#?(:clj 1 :corten 2) #?(:clj 3) [1 #?@(:corten [2 3]) 4]"), "2 [1 2 3 4]");
    assert_eq!(rt("#?(:clj 1 :cljs 2 :default 3) #?(:cljs 4 :default 5)"), "3 5");
    // Tags in branches not taken are not resolved.
    assert_eq!(rt("#?(:jank (#cpp x) :corten 5) #?(:corten 6 :jank #cpp y)"), "5 6");
    assert_eq!(rt("#\"a\\d\""), "(corten.kernel/regexp \"a\\\\d\")");
    let f = rt("#(+ % %3 %&)");
    assert!(f.starts_with("(fn* [p1__"), "{f}");
    assert!(f.contains(" & rest__"), "{f}");
    with_lisp(|| {
        let s = streams::make_string_input_stream(heap::string_node("^:dynamic ^{:doc \"d\"} *x*"));
        let x = reader::read(s).unwrap();
        let m = pr_str(clj::sym_meta(x));
        assert!(m.contains(":dynamic true") && m.contains(":doc \"d\""), "{m}");
    });
}

#[test]
fn syntax_quote() {
    let x = rt("`(if a ~b ~@c corten.kernel/car x# x#)");
    assert!(x.starts_with("(corten.kernel/concat (corten.kernel/list (quote if)) (corten.kernel/list (quote clojure.core/a)) (corten.kernel/list b) c (corten.kernel/list (quote corten.kernel/car))"), "{x}");
    let g: Vec<&str> = x.matches("x__").collect();
    assert_eq!(g.len(), 2);
    let names: Vec<&str> = x.split("(quote ").filter(|s| s.starts_with("x__")).map(|s| s.split(')').next().unwrap()).collect();
    assert_eq!(names[0], names[1], "same gensym within one syntax-quote");
    assert_eq!(rt("`[a ~b]"), "(corten.kernel/vec (corten.kernel/concat (corten.kernel/list (quote clojure.core/a)) (corten.kernel/list b)))");
    assert_eq!(rt("`:k `1 `\"s\""), ":k 1 \"s\"");
    // `cl/` is not special any more.
    assert_eq!(rt("cl/car"), "cl/car");
}
