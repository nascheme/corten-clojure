;;;; Clojure bootstrap, stage 0: the first macros, written on the special
;;;; forms and kernel functions only (the sequence functions of
;;;; src/clj/coll.rs, on the collections the reader makes). They are
;;;; replaced by clj/macros.clj.

(k/in-ns 'clojure.core$macros)
(corten.kernel/alias *ns* 'k (corten.kernel/find-ns 'corten.kernel))

;; (defmacro name doc? attr-map? [params*] body*) and the multi-arity
;; form. The macro gets &form and &env first.
(def ^:macro defmacro
  (fn* defmacro [&form &env name & body]
    (let* [body (if (k/string? (k/first body)) (k/next body) body)
           body (if (k/map? (k/first body)) (k/next body) body)
           add-env (fn* [arity]
                     (k/cons (k/vec (k/concat '[&form &env] (k/first arity)))
                             (k/rest arity)))]
      (k/list 'def (k/with-meta name {:macro true})
              (k/cons 'fn*
                      (if (k/vector? (k/first body))
                        (k/list (add-env body))
                        (loop* [bs body acc []]
                          (if bs
                            (recur (k/next bs) (k/conj acc (add-env (k/first bs))))
                            (k/seq acc)))))))))

(defmacro fn [& sigs] (k/cons 'fn* sigs))
(defmacro let [bindings & body] `(let* ~bindings ~@body))
(defmacro loop [bindings & body] `(loop* ~bindings ~@body))
(defmacro letfn [fnspecs & body]
  `(letfn* ~(loop* [s (k/seq fnspecs) acc []]
              (if s
                (recur (k/next s) (k/conj (k/conj acc (k/first (k/first s)))
                                          (k/cons 'fn* (k/first s))))
                acc))
           ~@body))

(defmacro when [test & body] `(if ~test (do ~@body)))
(defmacro when-not [test & body] `(if ~test nil (do ~@body)))
(defmacro if-not
  ([test then] `(if ~test nil ~then))
  ([test then else] `(if ~test ~else ~then)))

(defmacro and
  ([] true)
  ([x] x)
  ([x & next] `(let* [and# ~x] (if and# (and ~@next) and#))))

(defmacro or
  ([] nil)
  ([x] x)
  ([x & next] `(let* [or# ~x] (if or# or# (or ~@next)))))

(defmacro cond [& clauses]
  (if clauses
    (if (k/next clauses)
      (k/list 'if (k/first clauses) (k/first (k/next clauses))
              (k/cons `cond (k/next (k/next clauses))))
      (throw (k/make-instance (k/kernel-type "corten.lang/ArgumentError")
                              "cond requires an even number of forms" nil)))))

(defmacro declare [& names]
  `(do ~@(loop* [s names acc []]
           (if s (recur (k/next s) (k/conj acc (k/list 'def (k/first s)))) acc))))

(defmacro comment [& body] nil)

(defmacro -> [x & forms]
  (loop* [x x forms forms]
    (if forms
      (let* [f (k/first forms)
             threaded (if (k/seq? f)
                        (k/cons (k/first f) (k/cons x (k/rest f)))
                        (k/list f x))]
        (recur threaded (k/next forms)))
      x)))

(defmacro ->> [x & forms]
  (loop* [x x forms forms]
    (if forms
      (let* [f (k/first forms)
             threaded (if (k/seq? f)
                        (k/concat f (k/list x))
                        (k/list f x))]
        (recur threaded (k/next forms)))
      x)))

;; (defn name doc? attr-map? [params*] body*) without destructuring.
(defmacro defn [name & fdecl]
  (let* [fdecl (if (k/string? (k/first fdecl)) (k/next fdecl) fdecl)
         fdecl (if (k/map? (k/first fdecl)) (k/next fdecl) fdecl)]
    `(def ~name (fn* ~name ~@fdecl))))

(defmacro defn- [name & fdecl]
  `(defn ~(k/with-meta name {:private true}) ~@fdecl))

(defmacro in-ns [name] `(k/in-ns ~name))
