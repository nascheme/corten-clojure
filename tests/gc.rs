//! The garbage collector: boots the Clojure system (clj/init.clj) with a
//! small allocation threshold (many collections) and heap verification on.

use std::sync::Once;

use corten::clj::printer::pr_str;
use corten::error::catch_lisp_error;
use corten::{compiler, gc, heap, with_lisp};

static BOOT: Once = Once::new();

fn boot() {
    BOOT.call_once(|| {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/clj");
        std::env::set_current_dir(dir).unwrap();
        with_lisp(|| {
            heap::set_gc_params(256 << 10, true);
            let r = catch_lisp_error(|| compiler::clj_boot(heap::string_node("init.clj")));
            assert!(r.is_ok(), "init.clj failed: {r:?}");
        });
    });
}

/// Evaluate Clojure `src`; the last value printed, or "ERROR: ...".
fn ev(src: &str) -> String {
    boot();
    with_lisp(|| {
        let r = catch_lisp_error(|| pr_str(compiler::clj_eval_string(src)));
        r.unwrap_or_else(|e| format!("ERROR: {e}"))
    })
}

#[test]
fn boot_with_many_collections() {
    boot();
    let n = with_lisp(gc::gc_count);
    assert!(n > 20, "only {n} collections during boot");
    with_lisp(gc::verify_heap);
}

#[test]
fn collection_frees_garbage() {
    ev("(corten.kernel/gc)");
    let before = with_lisp(heap::bytes_used);
    ev("(dotimes [i 20000] (list 1 2 3 4 5 6 7 8 9 10))");
    ev("(corten.kernel/gc)");
    let after = with_lisp(heap::bytes_used);
    assert!(after < before + (256 << 10), "before {before}, after {after}");
}

#[test]
fn data_survives_collections() {
    assert_eq!(
        ev("(def gl (vec (map str (range 5000)))) (corten.kernel/gc) [(count gl) (first gl) (peek gl)]"),
        "[5000 \"0\" \"4999\"]"
    );
    assert_eq!(
        ev("(let [n (atom 0) f (fn [] (swap! n inc))] (dotimes [i 1000] (f) (vec (range 100))) (f))"),
        "1001"
    );
    assert_eq!(ev("(count (apply concat (repeat 300 (range 50))))"), "15000");
    assert_eq!(ev("(count (apply list (repeat 20000 1)))"), "20000");
}

#[test]
fn identity_hashes_survive_collections() {
    // goog/getUid keeps an EQ table that is rehashed after a GC moves its
    // keys; hash maps on JavaScript objects use it.
    assert_eq!(
        ev("(def objs (vec (repeatedly 2000 js-obj)))
            (def ids (mapv goog/getUid objs))
            (def m (zipmap objs (range)))
            (corten.kernel/gc)
            [(= ids (mapv goog/getUid objs)) (count (filter #(= (m (nth objs %)) %) (range 2000)))]"),
        "[true 2000]"
    );
}

#[test]
fn errors_during_compilation_reenable_gc() {
    // A macro expander throws while the compiler runs (GC inhibited); the
    // throw out of the compiler must re-enable GC.
    ev("(defmacro bad [] (throw (js/Error. \"bad macro\")))");
    assert!(ev("(defn uses-bad [] (bad))").starts_with("ERROR"));
    let n = with_lisp(gc::gc_count);
    ev("(corten.kernel/gc)");
    assert!(with_lisp(gc::gc_count) > n);
}
