//! The Clojure system (clj/init.clj).

use std::sync::Once;

use corten::clj::printer::pr_str;
use corten::error::catch_lisp_error;
use corten::{compiler, heap, with_lisp};

static BOOT: Once = Once::new();

fn boot() {
    BOOT.call_once(|| {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/clj");
        std::env::set_current_dir(dir).unwrap();
        with_lisp(|| {
            let r = catch_lisp_error(|| compiler::clj_boot(heap::string_node("init.clj")));
            assert!(r.is_ok(), "init.clj failed: {r:?}");
        });
    });
}

/// Evaluate Clojure `src`; the last value printed, or "error: ...".
fn cj(src: &str) -> String {
    boot();
    with_lisp(|| {
        // Tests run in any order; start each in the user namespace.
        let src = format!("(corten.kernel/in-ns 'user) {src}");
        let r = catch_lisp_error(|| pr_str(compiler::clj_eval_string(&src)));
        r.unwrap_or_else(|e| format!("error: {e}"))
    })
}

/// `cj` with identity hashes (`0x1f`, `@1f`) replaced by `0x_` and `@_`.
fn cj_ids(src: &str) -> String {
    let s = cj(src);
    let mut out = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        out.push(c);
        let hex_follows = (c == '@' || (c == 'x' && out.ends_with(" 0x")))
            && chars.peek().is_some_and(|d| d.is_ascii_hexdigit());
        if hex_follows {
            while chars.peek().is_some_and(|d| d.is_ascii_hexdigit()) {
                chars.next();
            }
            out.push('_');
        }
    }
    out
}

#[test]
fn truthiness_and_if() {
    assert_eq!(cj("[(if false 1 2) (if nil 1 2) (if 0 1 2) (if () 1 2) (if true 1)]"), "[2 2 1 1 1]");
    assert_eq!(cj("(if false 1)"), "nil");
    assert!(cj("(if)").contains("Too few arguments to if"));
}

#[test]
fn def_and_vars() {
    assert_eq!(cj("(def dv1 10) [dv1 (var dv1)]"), "[10 #'user/dv1]");
    assert_eq!(cj("(def dv2) (def dv2 \"doc\" 3) dv2"), "3");
    // As in ClojureScript an undeclared var only warns; it is unbound.
    assert!(cj("undefined-thing").contains("undefined-thing is unbound"));
    assert_eq!(cj("(def dv3 1) (set! dv3 2) dv3"), "2");
}

#[test]
fn var_api() {
    assert_eq!(cj("(def va1 1) (alter-var-root #'va1 + 10) [va1 (var? #'va1) (var-get #'va1)]"), "[11 true 11]");
    assert_eq!(
        cj("(def va2 1) (def log (atom [])) (add-watch #'va2 :k (fn [k r o n] (swap! log conj [k o n]))) \
            (def va2 2) (alter-var-root #'va2 inc) (remove-watch #'va2 :k) (def va2 5) @log"),
        "[[:k 1 2] [:k 2 3]]"
    );
    assert_eq!(cj("(def ^:dynamic *va3* 1) (binding [*va3* 2] (get-thread-bindings))"), "{#'user/*va3* 2}");
    assert_eq!(
        cj("(def ^:dynamic *va4* 1) (let [f (binding [*va4* 2] (bound-fn* (fn [] *va4*)))] [(f) *va4*])"),
        "[2 1]"
    );
    // alter-var-root inside binding changes the root, not the binding.
    assert_eq!(
        cj("(def ^:dynamic *va5* 1) [(binding [*va5* 2] (alter-var-root #'*va5* + 10) [*va5* (thread-bound? #'*va5*)]) *va5* (thread-bound? #'*va5*)]"),
        "[[2 true] 11 false]"
    );
    assert_eq!(cj("(defn va6 [] 1) [(with-redefs [va6 (fn [] 2)] (va6)) (va6)]"), "[2 1]");
    assert_eq!(cj("(def va7 1) (alter-meta! #'va7 assoc :foo 1) (select-keys (meta #'va7) [:foo :name])"), "{:foo 1, :name va7}");
    assert_eq!(cj("[(intern 'user 'va8 3) va8 (find-var 'user/va8) ((var inc) 1)]"), "[#'user/va8 3 #'user/va8 2]");
    assert_eq!(cj("(def va9 1) (ns-unmap *ns* 'va9) [(resolve 'va9) (contains? (ns-map 'user) 'map)]"), "[nil true]");
    assert_eq!(cj("(ns-resolve 'clojure.core 'map)"), "#'clojure.core/map");
    assert!(cj("(declare va10) (va10)").contains("#'user/va10 is unbound"));
}

#[test]
fn require_functions() {
    assert_eq!(cj("(require '[clojure.string :as rs1]) (rs1/upper-case \"a\")"), "\"A\"");
    assert_eq!(cj("(apply require ['[clojure.string :refer [join] :rename {join rj}]]) (rj \",\" [1 2])"), "\"1,2\"");
    assert_eq!(cj("(require '(clojure [set :as rs2])) (rs2/union #{1} #{2})"), "#{1 2}");
    assert_eq!(cj("(contains? (loaded-libs) 'clojure.string)"), "true");
    assert!(cj("(require 'no.such.lib)").contains("Could not locate no/such/lib"));
    assert_eq!(cj("(require '[fake.lib :as-alias fl]) [(find-ns 'fake.lib) ::fl/k]").split(' ').nth(1), Some(":fake.lib/k]"));
}

#[test]
fn let_and_closures() {
    assert_eq!(cj("(let* [a 1 b (corten.kernel/+ a 1)] [a b])"), "[1 2]");
    assert_eq!(cj("(let* [a 1] (let* [a 2 f (fn* [] a)] (f)))"), "2");
    // Closures created in a loop see their own iteration's value.
    assert_eq!(
        cj("(loop* [i 0 fs nil] (if (corten.kernel/< i 3) (recur (corten.kernel/+ i 1) (corten.kernel/cons (fn* [] i) fs)) (map (fn* [f] (f)) fs)))"),
        "(2 1 0)"
    );
    assert_eq!(cj("(def mk (fn* [x] (fn* [y] (corten.kernel/+ x y)))) ((mk 10) 5)"), "15");
}

#[test]
fn fn_arities_and_recur() {
    assert_eq!(
        cj("(def fa (fn* fa ([] (fa 1)) ([a] (corten.kernel/* a 10)) ([a b & more] [a b more]))) [(fa) (fa 2) (fa 1 2) (fa 1 2 3 4)]"),
        "[10 20 [1 2 nil] [1 2 (3 4)]]"
    );
    assert!(cj("(def fb (fn* ([a] a) ([a b c] c))) (fb 1 2)").contains("Wrong number of args (2)"));
    assert!(cj("((fn* [a] a))").contains("rong number of arg"));
    assert_eq!(cj("((fn* [n acc] (if (corten.kernel/= n 0) acc (recur (corten.kernel/- n 1) (corten.kernel/* acc n)))) 10 1)"), "3628800");
    assert_eq!(cj("((fn* [& xs] (if xs (recur (corten.kernel/next xs)) :done)) 1 2 3)"), ":done");
    assert!(cj("(loop* [a 1] (recur))").contains("Mismatched argument count to recur"));
    assert!(cj("(recur 1)").contains("Can't recur here"));
    assert_eq!(cj("(letfn* [ev (fn* [n] (if (corten.kernel/= n 0) true (od (corten.kernel/- n 1)))) od (fn* [n] (if (corten.kernel/= n 0) false (ev (corten.kernel/- n 1))))] [(ev 10) (ev 7)])"), "[true false]");
}

#[test]
fn macros_and_syntax_quote() {
    assert_eq!(cj("(defmacro unless1 [c & body] `(if ~c nil (do ~@body))) (unless1 false 1 2)"), "2");
    assert_eq!(cj("(defmacro m2 ([a] a) ([a b] `(corten.kernel/+ ~a ~b))) [(m2 3) (m2 3 4)]"), "[3 7]");
    assert_eq!(cj("(defmacro sw [x] `(let* [t# ~x] [t# t#])) (sw 5)"), "[5 5]");
    assert!(cj("unless1").contains("unless1 is unbound"));
    // A macro and a function can share a name; calls use the macro.
    assert_eq!(cj("(defmacro twice [x] `[~x ~x]) (def twice (fn* [x] (corten.kernel/* 2 x))) [(twice 3) (corten.kernel/funcall twice 3)]"), "[[3 3] 6]");
    assert_eq!(cj("(corten.kernel/macroexpand-1 '(unless1 a b))"), "(if a nil (do b))");
}

#[test]
fn try_catch_finally() {
    assert_eq!(cj("(try (throw 42) (catch :default e [:caught e]))"), "[:caught 42]");
    // Only errors are instances of the error types.
    assert_eq!(cj("(try (throw 42) (catch Error e :wrong) (catch :default e :right))"), ":right");
    assert_eq!(
        cj("(try (throw (StateError. \"m\")) (catch ArgumentError e :wrong) (catch Error e (ex-message e)))"),
        "\"m\""
    );
    assert_eq!(cj("(try (corten.kernel/name 1) (catch Error e :kernel))"), ":kernel");
    assert_eq!(cj("(try (corten.kernel/name 1) (catch :default e :cl-error))"), ":cl-error");
    assert_eq!(cj("(def tlog nil) [(try 1 (finally (set! tlog :fin))) tlog]"), "[1 :fin]");
    assert_eq!(
        cj("(def tl2 nil) [(try (try (throw :x) (finally (set! tl2 :inner))) (catch :default e e)) tl2]"),
        "[:x :inner]"
    );
    let ty = "(def MyErr (corten.kernel/make-type \"MyErr\" nil))";
    assert_eq!(cj(&format!("{ty} (try (throw :y) (catch MyErr e :wrong) (catch :default e [:right e]))")), "[:right :y]");
    assert!(cj("(throw :z)").contains("Clojure exception: :z"));
}

#[test]
fn namespaces() {
    assert_eq!(
        cj("(corten.kernel/in-ns 'nstest.a) (def x 1) (corten.kernel/in-ns 'user) nstest.a/x"),
        "1"
    );
    assert_eq!(
        cj("(corten.kernel/in-ns 'nstest.b) (corten.kernel/alias *ns* 'a (corten.kernel/find-ns 'nstest.a)) (def y [a/x ::k ::a/k]) (corten.kernel/in-ns 'user) nstest.b/y"),
        "[1 :nstest.b/k :nstest.a/k]"
    );
    // A new namespace refers clojure.core.
    assert_eq!(cj("(corten.kernel/in-ns 'nstest.c) (def z (vector 1)) (corten.kernel/in-ns 'user) nstest.c/z"), "[1]");
}

#[test]
fn kernel_interop() {
    // Kernel functions are the vars of corten.kernel.
    assert_eq!(cj("(corten.kernel/apply (fn* [& xs] (corten.kernel/count xs)) (corten.kernel/list 1 2 3))"), "3");
    assert_eq!(cj("(corten.kernel/funcall corten.kernel/list 1 2)"), "(1 2)");
    // apply gives a variadic fn the rest of the seq unrealized.
    assert_eq!(cj("(apply (fn [a & r] [a (first r) (second r)]) (range))"), "[0 1 2]");
    assert_eq!(cj("(apply (fn ([a] 1) ([a b & r] r)) 1 2 3 [4 5])"), "(3 4 5)");
    assert_eq!(cj("(apply (fn ([a] 1) ([a b & r] r)) [1 2])"), "nil");
    assert_eq!(cj("(apply (fn ([a] 1) ([a b & r] r)) [1])"), "1");
    assert_eq!(cj("(apply str 1 2 [3])"), "\"123\"");
    assert_eq!(cj("(take 3 (apply concat (repeat [1 2])))"), "(1 2 1)");
}

#[test]
fn instances_protocols_interop() {
    let setup = r#"
(def Pt (corten.kernel/make-type "Pt" (corten.kernel/list "x" "y")))
(def p (new Pt 1 2))
(def getx-id (corten.kernel/new-method-id 'getx))
(corten.kernel/extend Pt getx-id (fn* [this] (fields* this [x ^:mutable y] (set! y (corten.kernel/+ y 1)) [x y])))
(corten.kernel/add-obj-method Pt "sum" (fn* [this a] (fields* this [x y] (corten.kernel/+ x y a))))
"#;
    assert_eq!(cj(&format!("{setup} [(.-x p) (. p -y) (. p (sum 1)) (.sum p 2)]")), "[1 2 4 5]");
    assert_eq!(cj(&format!("{setup} (set! (.-x p) 10) [((corten.kernel/method getx-id p) p) (.-y p)]")), "[[10 3] 3]");
    assert_eq!(cj_ids(&format!("{setup} [(Pt. 3 4) (corten.kernel/type-of (Pt. 3 4))]")), "[#object[Pt 0x_ \"Pt@_\"] Pt]");
    assert!(cj(&format!("{setup} (Pt. 1)")).contains("Wrong number of args (1) passed to constructor of Pt"));
    assert!(cj(&format!("{setup} ((corten.kernel/method getx-id 5) 5)")).contains("No protocol method getx defined for type corten.lang/Long"));
    // As in JavaScript, a missing property is nil.
    assert_eq!(cj(&format!("{setup} [(.-z p) (identical? (.-constructor p) Pt)]")), "[nil true]");
    // A default implementation applies to every type.
    assert_eq!(
        cj(&format!("{setup} (corten.kernel/extend (corten.kernel/builtin-type 'default) getx-id (fn* [x] :dflt)) ((corten.kernel/method getx-id 5) 5)")),
        ":dflt"
    );
}

#[test]
fn invoking_instances() {
    let setup = r#"
(deftype Fx [v] IFn (-invoke [this] v) (-invoke [this a] a) (-invoke [this a b c] [a b c]))
(def f (Fx. :v))
"#;
    assert_eq!(
        cj(&format!("{setup} [(:a {{:a 1}}) ({{:a 2}} :a) (f) (f 1) (f 1 2 3) (corten.kernel/funcall f 7) (corten.kernel/apply f (corten.kernel/list 8))]")),
        "[1 2 :v 1 [1 2 3] 7 8]"
    );
    assert!(cj(&format!("{setup} ((corten.kernel/make-type \"T\" nil))")).contains("Not a function"));
}

#[test]
fn core_macros() {
    let setup = r#"
(ns user.macros-test)
(defprotocol IShape (area [s]) (scale [s k] [s k j]))
(deftype Sq [^:mutable side]
  IShape
  (area [_] (corten.kernel/* side side))
  (scale [this k] (set! side (corten.kernel/* side k)) this)
  (scale [this k j] [k j])
  Object
  (toString [_] "sq"))
(def s (Sq. 3))
(defprotocol P2 (hello [x]))
(extend-type number P2 (hello [x] [:num x]))
(extend-type default P2 (hello [x] :other))
(def r (reify P2 (hello [_] :reified)))
"#;
    assert_eq!(
        cj(&format!("{setup} [(area s) (area (scale s 2)) (scale s 1 2) (.toString s) (hello 5) (hello \"x\") (hello r)]")),
        "[9 36 [1 2] \"sq\" [:num 5] :other :reified]"
    );
    assert_eq!(
        cj(&format!("{setup} [(satisfies? IShape s) (satisfies? IShape 5) (instance? Sq s) (instance? Sq 5)]")),
        "[true false true false]"
    );
    // A method body sees the protocol fn under the method's name, and
    // parameters shadow fields of the same name.
    assert_eq!(
        cj(&format!("{setup} (deftype Wrap [k] P2 (hello [_] (hello k)) IShape (scale [_ k] k)) [(hello (Wrap. 5)) (scale (Wrap. 1) 2)]")),
        "[[:num 5] 2]"
    );
    assert_eq!(cj("[(case 2 1 :a (2 3) :b :c) (case \"x\" \"x\" :s :d) (case 'foo foo :sym :d) (case 9 1 :a :d)]"), "[:b :s :sym :d]");
    assert!(cj("(case 9 1 :a)").contains("No matching clause: 9"));
    assert_eq!(cj("[(cond-> 1 true (corten.kernel/+ 1) false (corten.kernel/* 10)) (-> 1 inc inc) (->> 2 (corten.kernel/- 10)) (condp corten.kernel/= 2 1 :one 2 :two)]"), "[2 3 8 :two]");
    assert_eq!(cj("[(when-let [x 5] x) (if-let [x nil] x :no) (if-some [x false] x :no) (some-> 1 inc) (some-> nil inc)]"), "[5 :no false 2 nil]");
    assert_eq!(cj("[(loop [i 0] (if (< i 3) (recur (inc i)) i)) (dotimes [i 3] i) (let [a 1 b (inc a)] [a b])]"), "[3 nil [1 2]]");
    assert_eq!(cj("(def ^:dynamic *dyn* 1) (defn get-dyn [] *dyn*) [(binding [*dyn* 2] (get-dyn)) (get-dyn)]"), "[2 1]");
    assert_eq!(cj("(defonce once 1) (defonce once 2) once"), "1");
    assert_eq!(cj("(defn f1 \"doc\" {:a 1} ([] 0) ([x] x)) [(f1) (f1 7)]"), "[0 7]");
    assert_eq!(cj("[(+ 1 2 3) (- 10 1 2) (* 2 3) (< 1 2 3) (> 1 2) (bit-and 12 10) (bit-shift-left 1 4) (max 1 5 3)]"), "[6 7 6 true false 8 16 5]");
}

#[test]
fn read_form_print_hooks() {
    // Quoted literals and read-string give Clojure data.
    assert_eq!(cj("(map vector? ['[1] (read-string \"[1]\")])"), "(true true)");
    assert_eq!(cj("[(map? '{:a 1}) (set? '#{1}) (list? '(1 (2))) (type '[]) '[1 {:a #{2}} (3 ())]]"),
        "[true true true clojure.core/PersistentVector [1 {:a #{2}} (3 ())]]");
    assert_eq!(cj("(meta (first (read-string \"[^:mutable side]\")))"), "{:mutable true}");
    // Macros defined now get Clojure data and may return it.
    assert_eq!(cj("(defmacro hm [v] (if (vector? v) (conj v (count v)) :no)) [(hm [1 (inc 1)]) (hm 1)]"), "[[1 2 2] :no]");
    assert_eq!(cj("(defmacro hf [& _] (list 'quote &form)) (list? (hf [1] ()))"), "true");
    // The printer prints vars and namespaces inside collections.
    assert_eq!(cj_ids("(def hv 1) [(var hv) (atom 1)]"), "[#'user/hv #object[clojure.core.Atom 0x_ {:status :ready, :val 1}]]");
}

#[test]
fn cljs_test_support() {
    // identical? is JavaScript's ===: equal strings are identical, so
    // symbols compare by name.
    assert_eq!(cj("[(= 'a 'a) (identical? \"ab\" (str \"a\" \"b\")) (= 'a 'b)]"), "[true true false]");
    // ex-info is a js/Error; CL errors are caught as js/Error.
    assert_eq!(cj("(try (throw (ex-info \"x\" {})) (catch js/Error e (ex-message e)))"), "\"x\"");
    assert_eq!(cj("(try (nth [1] 5) (catch js/Error e (ex-message e)))"), "\"No item 5 in vector of length 1\"");
    assert_eq!(cj("(eval (list 'let '[x 2] '[x x]))"), "[2 2]");
    // Tagged literals and Type.FIELD.
    assert_eq!(cj("[#queue [1 2] #inst \"2010-11-12T13:14:15.666-05:00\" (= #uuid \"a\" #uuid \"a\")]"),
        "[#queue [1 2] #inst \"2010-11-12T18:14:15.666-00:00\" true]");
    assert_eq!(cj("(count cljs.core.PersistentQueue.EMPTY)"), "0");
    // Strings are seqs of characters.
    assert_eq!(cj("[(seq \"ab\") (nth \"ab\" 1) (get \"ab\" 0)]"), "[(\\a \\b) \\b \\a]");
    // Multimethods; macros referred with :refer; an unknown namespace
    // is an error (it used to intern the name in the current namespace).
    assert_eq!(cj("(defmulti mm :k) (defmethod mm :a [_] 1) (defmethod mm :default [_] 2) [(mm {:k :a}) (mm {})]"), "[1 2]");
    assert_eq!(cj("(require '[cljs.test :refer [is]]) (macroexpand-1 '(is true))"), "(cljs.test/is true nil)");
    assert!(cj("(fn [] (nosuchns/get 1))").contains("No such namespace"));
    assert_eq!(cj("(require '[clojure.string :as s]) [(s/join \",\" [1 2]) (s/escape \"a<b\" {\\< \"&lt;\"}) (s/trim \" a \")]"),
        "[\"1,2\" \"a&lt;b\" \"a\"]");
}

#[test]
fn cljs_core_test_fixes() {
    // aset past the end grows an array, as in JavaScript.
    assert_eq!(cj_ids("[(let [a (array)] (aset a 2 :x) a) (sequence (map inc) [1 2])]"), "[#object[corten.lang.Array 0x_ [nil nil :x]] (2 3)]");
    // Syntax-quote leaves .method symbols alone.
    assert_eq!(cj("(ns sq.test) (with-out-str (print 1))"), "\"1\"");
    // Trailing map for keyword arguments; hierarchies of types; @#'v.
    assert_eq!(cj("(defn tk [& {:keys [a b]}] [a b]) [(tk {:a 1 :b 2}) (tk :a 1)]"), "[[1 2] [1 nil]]");
    assert_eq!(cj("(isa? (derive (make-hierarchy) (type {}) ::m) (type {}) ::m)"), "true");
    assert_eq!(cj("(def dv 3) [@#'dv (nth (range) 40)]"), "[3 40]");
}

#[test]
fn records_and_metadata() {
    assert_eq!(
        cj("(defrecord R [a b]) (def r (->R 1 2)) [r (:a r) (assoc r :c 3) (dissoc r :a) (= r (map->R {:a 1 :b 2})) (count r) (record? r) (= r (assoc r :a 2))]"),
        "[#user.R{:a 1, :b 2} 1 #user.R{:a 1, :b 2, :c 3} {:b 2} true 2 true false]"
    );
    // Metadata on collection literals, fn forms and quoted symbols.
    assert_eq!(cj("[(meta ^{\"x\" \"y\"} []) (meta ^{:foo true} (fn [])) (meta '^:bar foo) (meta (eval (read-string \"^:a [1]\")))]"),
        "[{\"x\" \"y\"} {:foo true} {:bar true} {:a true}]");
    // Var metadata; implements? does not count default.
    assert_eq!(cj("(defn mtf \"doc\" {:k 1} [a]) (select-keys (meta #'mtf) [:doc :k :arglists :name :ns])"),
        "{:doc \"doc\", :k 1, :arglists ([a]), :name mtf, :ns user}");
    assert_eq!(cj("[(var? #'mtf) (implements? IHash \"y\") (= (hash \"y\") (hash (str \"y\"))) (instance? js/Error (ex-info \"a\" {}))]"),
        "[true false true true]");
}

#[test]
fn namespaces_vars_and_reify_meta() {
    // Calling a var calls its value; resolve and ns-publics.
    assert_eq!(cj("[((resolve 'first) [1 2]) (#'inc 1) (resolve 'no-such-thing)]"), "[1 2 nil]");
    assert_eq!(cj("(require 'clojure.string) [(find (ns-publics 'clojure.string) 'join) (contains? (ns-imports 'clojure.string) 'StringBuffer)]"),
        "[[join #'clojure.string/join] true]");
    // reify supports metadata; defn does not bind its name in the body.
    assert_eq!(cj("(defprotocol IFoo (foo [this])) (let [r (with-meta (reify IFoo (foo [_] :foo)) {:a 1})] [(meta r) (foo r)])"),
        "[{:a 1} :foo]");
    assert_eq!(cj("[(let [w 1] ((defn w [] w))) (:k \"str\")]"), "[1 nil]");
}

#[test]
fn numbers_dates_and_kwargs() {
    // Clojure numbers: ratios, BigInt contagion, Long overflow.
    assert_eq!(cj("[(/ 2) (/ 6 2) (/ 1.0 0) (/ 0.0 0) (+ 1/2 1/2) (+' 9223372036854775807 1) (* 2N 3)]"),
        "[1/2 3 ##Inf ##NaN 1N 9223372036854775808N 6N]");
    assert_eq!(cj("[(try (/ 1 0) (catch Error e :div0)) (try (inc 9223372036854775807) (catch Error e :overflow))]"),
        "[:div0 :overflow]");
    assert_eq!(cj("[(= 1 1N) (= 1 1.0) (== 1 1.0) (= (hash 1) (hash 1N)) (int? 1N) (integer? 1N) (bit-shift-left 1 63)]"),
        "[true false true true false true -9223372036854775808]");
    assert_eq!(cj("[(js/Date. \"2010-11-12T13:14:15.666-05:00\") (js/Date. 0)]"),
        "[#inst \"2010-11-12T18:14:15.666-00:00\" #inst \"1970-01-01T00:00:00.000-00:00\"]");
    assert_eq!(cj("(defn kw [n & {:keys [a b]}] (+ n a b)) [(kw 100 :a 1 {:b 2}) (kw 1 {:a 1 :b 1})]"), "[103 3]");
    assert_eq!(cj("[(subseq (sorted-set 1 2 3) >= 12) (defrecord P2 [a]) (pr-str (P2. 1))]"), "[nil user/P2 \"#user.P2{:a 1}\"]");
}

#[test]
fn syntax_quote_data_and_special_forms() {
    // Syntax-quoted collections evaluated at run time are Clojure data.
    assert_eq!(cj("(let [x 1] [`[~x] `{:a ~x} `#{~x} (vector? `[~x]) (map? `{:a ~x})])"),
        "[[1] {:a 1} #{1} true true]");
    // () inside a quoted map is the empty list.
    assert_eq!(cj("(list? (:a '{:a ()}))"), "true");
    // A macro named like a special form does not shadow it.
    assert_eq!(cj("(ns sq-def (:refer-clojure :exclude [def])) (defmacro def [x] :mine) (defmacro m2 [] 2) [(m2) (sq-def/def 1)]"),
        "[2 :mine]");
}

#[test]
fn stack_overflow_is_an_error() {
    assert_eq!(cj("(defn so-f [n] (+ 1 (so-f (inc n)))) (try (so-f 0) (catch :default e (ex-message e)))"),
        "\"Stack overflow (deep recursion)\"");
    assert_eq!(cj("(defn so-g [n] (if (zero? n) :done (so-g (dec n)))) (so-g 1000)"), ":done");
}

#[test]
fn load_string_and_file() {
    assert_eq!(cj("(load-string \"(def ls1 2) (* ls1 3)\")"), "6");
    // load-string restores *ns*.
    assert_eq!(cj("(load-string \"(in-ns 'ls.other)\") (ns-name *ns*)"), "user");
    let f = std::env::temp_dir().join(format!("clj-load-{}.clj", std::process::id()));
    std::fs::write(&f, "(def lf1 [1 2])").unwrap();
    assert_eq!(cj(&format!("(load-file {:?}) lf1", f.to_str().unwrap())), "[1 2]");
    std::fs::remove_file(&f).unwrap();
}

#[test]
fn try_compiles_to_ir() {
    // No matching catch clause: rethrown to the outer try.
    assert_eq!(cj("(defn ti1 [] (try (throw (js/Error. \"a\")) (catch ExceptionInfo e :wrong))) (try (ti1) (catch js/Error e (ex-message e)))"), "\"a\"");
    // A closure captures the caught value; a throw from a closure called
    // in the body is caught.
    assert_eq!(cj("[((try (throw 5) (catch :default e (fn [] e)))) (try ((fn [] (throw 6))) (catch :default e e))]"), "[5 6]");
    // finally runs when a throw passes through, before the outer handler.
    assert_eq!(cj("(def tfl (atom [])) (try (try (throw :x) (finally (swap! tfl conj :f))) (catch :default e (swap! tfl conj e))) @tfl"), "[:f :x]");
    // try in a loop, with recur outside it.
    assert_eq!(cj("(loop [i 0 acc []] (if (< i 3) (recur (inc i) (conj acc (try (if (odd? i) (throw i) i) (catch :default e [e])))) acc))"), "[0 [1] 2]");
}

#[test]
fn native_types() {
    assert_eq!(
        cj("(map type [1 1.5 1/2 100000000000000000000 \"a\" \\a true inc nil])"),
        "(corten.lang/Long corten.lang/Double corten.lang/Ratio corten.lang/BigInt corten.lang/String \
         corten.lang/Character corten.lang/Boolean corten.lang/Function nil)"
    );
    assert_eq!(cj("[(instance? Number 1) (instance? Long 1.5) (instance? String \"a\") (= Long (class 1))]"), "[true false true true]");
    assert_eq!(cj("[(type (with-meta [] {:type :t})) (class (with-meta [] {:type :t}))]"), "[:t clojure.core/PersistentVector]");
    assert_eq!(cj("(defprotocol PN (pn [x])) (extend-type number PN (pn [x] :n)) [(pn 1) (pn 1/2)]"), "[:n :n]");
}

#[test]
fn type_hierarchies() {
    assert_eq!(
        cj("[(parents ArgumentError) (ancestors Long) (isa? IndexError Error) (isa? Number Long) (isa? [Long] [Number])]"),
        "[#{corten.lang/Error} #{corten.lang/Number} true false true]"
    );
    assert_eq!(
        cj("(defmulti mf class) (defmethod mf Number [x] :n) (defmethod mf :default [x] :d) [(mf 1) (mf 1/2) (mf \"a\")]"),
        "[:n :n :d]"
    );
}

#[test]
fn native_methods() {
    assert_eq!(
        cj("[(.toUpperCase \"ab\") (.charAt \\a 0) (String/fromCharCode 65) (.join (array 1 2) \"-\") (.toString 255 16) (.call inc nil 1)]"),
        "[\"AB\" \"a\" \"A\" \"1-2\" \"ff\" 2]"
    );
    // Call-site caches follow method changes.
    assert_eq!(
        cj("(deftype MT [] Object (m [_] 1)) (defn callm [x] (.m x)) (def r1 (callm (MT.))) \
            (corten.kernel/add-obj-method MT \"m\" (fn [_] 2)) [r1 (callm (MT.)) (map #(.toUpperCase %) [\"a\" \\b])]"),
        "[1 2 (\"A\" \"B\")]"
    );
}

#[test]
fn corten_functions() {
    assert_eq!(
        cj("[(corten.string/index-of \"abc\" \"c\") (corten.string/index-of \"abc\" \"x\") (corten.string/substring \"abcd\" 1 3) \
            (corten.string/starts-with? \"abc\" \"ab\") (corten.string/length \"abc\") (corten.number/to-string 255 16) \
            (vec (corten.array/sort! (corten.array/push! (array 3 1) 2 0) compare)) (corten.array/join (array 1 nil 2) \"-\")]"),
        "[2 nil \"bc\" true 3 \"ff\" [0 1 2 3] \"1--2\"]"
    );
    assert!(cj("(corten.string/length 1)").contains("Not a string"));
    assert!(cj("(corten.string/length)").contains("Wrong number of args (0) passed to: corten.string/length"));
}

#[test]
fn typed_arrays_and_vector_of() {
    // Typed arrays: aset checks and coerces; the size is fixed; aclone
    // keeps the element type.
    assert_eq!(cj("(let [a (double-array 2)] (aset a 0 1) (vec a))"), "[1.0 0.0]");
    assert!(cj("(aset (long-array 2) 0 1.5)").contains("Cannot store 1.5 in a long array"));
    assert!(cj("(aset (int-array 2) 0 3000000000)").contains("out of range for int"));
    assert!(cj("(aset (long-array 2) 5 1)").contains("out of bounds"));
    assert!(cj("(aset (aclone (char-array 1)) 0 65)").contains("Cannot store 65 in a char array"));
    // vector-of: conj and assoc coerce.
    assert_eq!(cj("[(conj (vector-of :long 1.5) 2.5) (assoc (vector-of :double 1) 0 2) (vector-of :int \\a)]"),
        "[[1 2] [2.0] [97]]");
    assert!(cj("(vector-of :int nil)").contains("Not a number"));
}

#[test]
fn format_math_and_error_types() {
    assert_eq!(
        cj("(require 'clojure.string) [(format \"%5d|%-4s|%,.2f|%.1f|%x|%e|%g|%s\" 42 :a 1234.567 0.25 -1 12345.678 0.0001 nil) \
            (clojure.string/replace \"food\" #\"(o)\" \"[$1\\\\$]\")]"),
        "[\"   42|:a  |1,234.57|0.3|ffffffffffffffff|1.234568e+04|0.000100000|null\" \"f[o$][o$]d\"]"
    );
    assert_eq!(
        cj("(require '[clojure.math :as m]) [(m/round 2.5) (m/floor-div -2 5) (m/signum -0.0) (m/pow 2 3) \
            (try (m/add-exact Long/MAX_VALUE 1) (catch ArithmeticError _ :overflow))]"),
        "[3 -1 -0.0 8.0 :overflow]"
    );
    assert_eq!(
        cj("(defn f [x]) (defmacro m [a] a) \
            [(try (/ 1 0) (catch ArithmeticError _ :arith)) (try (+ 1 nil) (catch TypeError e (ex-message e))) \
             (try (aget (int-array 1) 3) (catch IndexError _ :index)) \
             (try (f) (catch ArityError e [(.-actual e) (.-name e)])) \
             (try (macroexpand '(m)) (catch ArityError e (ex-message e)))]"),
        "[:arith \"Not a number: nil\" :index [0 \"user/f\"] \"Wrong number of args (0) passed to: user/m\"]"
    );    assert_eq!(
        cj("[Long/MAX_VALUE Boolean/TRUE (try (eval 'Long/FOO) (catch Error e (ex-message e)))]"),
        "[9223372036854775807 true \"Unable to find static field: FOO in corten.lang/Long\"]"
    );    assert_eq!(
        cj("(deftype T [] IFn (-invoke [_ x] x)) \
            (mapv #(try (%) (catch ArityError e (ex-message e))) [#(:a) #({} 1 2 3) #((T.)) #(apply :a (range 21))])"),
        "[\"Wrong number of args (0) passed to: :a\" \"Wrong number of args (3) passed to: clojure.core/PersistentArrayMap\" \
          \"Wrong number of args (0) passed to: user/T\" \"Wrong number of args (> 20) passed to: :a\"]"
    );
}

#[test]
fn out_and_err_writers() {
    assert_eq!(
        cj("(require '[clojure.pprint :as pp]) \
            [(with-out-str (print :a) (.write *out* \"b\") (flush)) \
             (let [w (StringWriter.)] (binding [*out* w] (prn [1])) (str w)) \
             (with-out-str (binding [*out* *err*] (print 1))) \
             (with-out-str (pp/pprint {:a 1}) (pp/cl-format true \"~a\" 2))]"),
        "[\":ab\" \"[1]\\n\" \"\" \"{:a 1}\\n2\"]"
    );
}

#[test]
fn readers() {
    assert_eq!(
        cj("(require 'clojure.edn) \
            [(with-in-str \"(a) ^:m [1]\" [(read) (meta (read)) (read *in* false :eof)]) \
             (with-in-str \"x\\ny\\r\\nz\" [(read-line) (read-line) (read-line) (read-line)]) \
             (line-seq (BufferedReader. (StringReader. \"a\\nb\"))) \
             (let [r (PushbackReader. (StringReader. \"{:a 1} ::x\"))] \
               [(clojure.edn/read r) (try (clojure.edn/read r) (catch Error e :invalid))])]"),
        "[[(a) {:m true} :eof] [\"x\" \"y\" \"z\" nil] (\"a\" \"b\") [{:a 1} :invalid]]"
    );
}
