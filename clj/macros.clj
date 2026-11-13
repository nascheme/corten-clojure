;;;; Clojure bootstrap, stage 2: the clojure.core macros, in
;;;; clojure.core$macros. Macro bodies run on clj.boot; expansions refer
;;;; to clojure.core functions, which cljs.core defines. Adapted from
;;;; Clojure's core.clj and ClojureScript's core.cljc; js* templates are
;;;; replaced by kernel functions.

(in-ns 'clojure.core$macros)
(k/refer *ns* (k/find-ns 'clj.boot))

(defn- argument-error
  "A corten.lang/ArgumentError (a kernel type)."
  [msg]
  (k/make-instance (k/kernel-type "corten.lang/ArgumentError") msg nil))

(defn- check-args [form & pairs]
  (loop [ps pairs]
    (when ps
      (when-not (first ps)
        (throw (argument-error (str (first form) " requires " (second ps) " in " (pr-str form)))))
      (recur (nnext ps)))))

;;; Destructuring: cljs's destructure (cljs/core.cljc, with the cljs
;;; branches of the reader conditionals), on clj.boot.

(defn- destvec* [pb bvec b val]
  (let* [gvec (gensym "vec__")
         gseq (gensym "seq__")
         gfirst (gensym "first__")
         has-rest (some (fn* [x] (= x '&)) b)]
    (loop* [ret (let* [ret (conj bvec gvec val)]
                  (if has-rest
                    (conj ret gseq (list `seq gvec))
                    ret))
            n 0
            bs b
            seen-rest? false]
      (if (seq bs)
        (let* [firstb (first bs)]
          (cond
            (= firstb '&) (recur (pb ret (second bs) gseq)
                                 n
                                 (nnext bs)
                                 true)
            (= firstb :as) (pb ret (second bs) gvec)
            :else (if seen-rest?
                    (throw (argument-error "Unsupported binding form, only :as can follow & parameter"))
                    (recur (pb (if has-rest
                                 (conj ret
                                       gfirst `(first ~gseq)
                                       gseq `(next ~gseq))
                                 ret)
                               firstb
                               (if has-rest
                                 gfirst
                                 (list `nth gvec n nil)))
                           (inc n)
                           (next bs)
                           seen-rest?))))
        ret))))

(defn- destmap* [pb bvec b v]
  (let* [gmap (gensym "map__")
         gtemp (gensym "temp__")
         defaults (get b :or)
         b (dissoc b :defaults)
         gdefaults (when defaults (zipmap (keys defaults) (map (fn* [_] (gensym "default__")) (keys defaults))))
         select (get b :select)
         all (get b :all)
         excess (get b :excess)
         missing (get b :missing)
         gnotfound (when missing (gensym "notfound__"))
         gnotfound? (when missing (gensym "notfound?__"))
         xf (fn* [mk]
              (let* [mkns (namespace mk)
                     mkn (name mk)]
                (cond
                  (corten.string/starts-with? mkn "keys") (fn* [x] (keyword (or mkns (namespace x)) (name x)))
                  (corten.string/starts-with? mkn "syms") (fn* [x] (list `quote (symbol (or mkns (namespace x)) (name x))))
                  (corten.string/starts-with? mkn "strs") str
                  :else (throw (argument-error (str "Unsupported map directive: " mk))))))
         ret (reduce (fn* [ret e]
                       (conj ret (val e) (get defaults (key e))))
                     bvec gdefaults)
         ret (conj ret gmap v gmap `(--destructure-map ~gmap))
         ret (if (get b :as) (conj ret (get b :as) gmap) ret)
         ret (if missing
               (conj ret
                     missing nil
                     gnotfound (list `js-obj))
               ret)
         bes (dissoc b :as :or :select :all :excess :missing)
         localize (fn* [bb]
                    (if (ident? bb)
                      (with-meta (symbol nil (name bb)) (meta bb)) bb))
         push1 (fn* [ret bb bk req?]
                 (let* [local (localize bb)
                        local-default? (contains? defaults local)
                        key-default? (contains? defaults bk)
                        bv (if (or local-default? key-default?)
                             (if (and local-default? key-default?)
                               (throw (argument-error
                                       (str "Multiple :or defaults for same key: " bk " '" local "'")))
                               (if req?
                                 (throw (argument-error
                                         (str "Can't supply default value for required key: " bk)))
                                 (list `get gmap bk (if local-default? (get gdefaults local) (get gdefaults bk)))))
                             (if req?
                               (if missing
                                 (list `get gmap bk gnotfound)
                                 (list `req! gmap bk))
                               (list `get gmap bk)))]
                   (if (ident? bb)
                     (if (and req? missing)
                       (conj ret
                             gtemp bv
                             gnotfound? `(identical? ~gtemp ~gnotfound)
                             missing `(if ~gnotfound? (assoc ~missing ~bk nil) ~missing)
                             local `(when-not ~gnotfound? ~gtemp))
                       (conj ret local bv))
                     (pb ret bb bv))))
         retsel
         (loop* [ret ret sel #{} bes (seq bes) b->k {} subs nil suba nil subexcess nil submissing nil]
           (if bes
             (let* [be (first bes) bb (key be) bk (val be)]
               (if (keyword? bb)
                 (let* [dir bb
                        tr (xf bb)
                        req? (corten.string/ends-with? (name bb) "!")
                        retsel
                        (loop* [ret ret sel sel bbs (seq bk) preamp? true b->k b->k]
                          (if bbs
                            (let* [bb (first bbs)]
                              (if (= bb '&)
                                (if preamp?
                                  (recur ret sel (next bbs) false b->k)
                                  (throw (argument-error (str "& can only appear once in " dir))))
                                (let* [_ (when (and (not preamp?) (symbol? bb))
                                           (throw (argument-error
                                                   (str "'" bb
                                                        "' - binding symbols can only appear before '&', use keys after"))))
                                       bk (if preamp? (tr bb) bb)]
                                  (recur (if (or preamp? req?)
                                           (push1 ret (if preamp? bb gtemp) bk req?)
                                           ret)
                                         (conj sel bk)
                                         (next bbs) preamp?
                                         (if preamp? (assoc b->k (localize bb) bk) b->k)))))
                            {:ret ret :sel sel :b->k b->k}))]
                   (recur (get retsel :ret) (get retsel :sel) (next bes) (get retsel :b->k)
                          subs suba subexcess submissing))
                 (let* [subsel? (and select (map? bb))
                        bb (if (or (not subsel?) (get bb :select))
                             bb
                             (assoc bb :select (gensym "select__")))
                        subs (if subsel? (assoc subs bk (get bb :select)) subs)
                        suball? (and all (map? bb))
                        bb (if (or (not suball?) (get bb :all))
                             bb
                             (assoc bb :all (gensym "all__")))
                        suba (if suball? (assoc suba bk (get bb :all)) suba)
                        subexcess? (and excess (map? bb))
                        bb (if (or (not subexcess?) (get bb :excess))
                             bb
                             (assoc bb :excess (gensym "excess__")))
                        subexcess (if subexcess? (assoc subexcess bk (get bb :excess)) subexcess)
                        submissing? (and missing (map? bb))
                        bb (if (or (not submissing?) (get bb :missing))
                             bb
                             (assoc bb :missing (gensym "missing__")))
                        submissing (if submissing? (assoc submissing bk (get bb :missing)) submissing)
                        b->k (if (symbol? bb) (assoc b->k bb bk) b->k)]
                   (recur (push1 ret bb bk false) (conj sel bk) (next bes) b->k
                          subs suba subexcess submissing))))
             {:ret ret :sel sel :b->k b->k :subs subs :suba suba
              :subexcess subexcess :submissing submissing}))
         ret (get retsel :ret)
         sel (vec (get retsel :sel))
         b->k (get retsel :b->k)
         new-or-code (and defaults (or select all))
         bk (fn* [x]
              (if (symbol? x)
                (let* [bk (get b->k x)]
                  (when (and new-or-code (not bk))
                    (throw (argument-error (str "symbol " x " in :or does not refer to a binding"))))
                  bk)
                x))
         dm (when defaults (zipmap (map bk (keys gdefaults)) (vals gdefaults)))
         _ (and new-or-code (not= (count (select-keys dm sel)) (count defaults))
                (throw (argument-error (str "keys "
                                            (pr-str (apply disj (set (keys dm)) sel))
                                            " appear only in :or"))))
         dm (if (empty? dm) nil dm)
         ret (if select
               (conj ret select `(when-let [mm# (merge ~dm ~gmap (some-vals ~(get retsel :subs)))]
                                   (select-keys mm# ~sel)))
               ret)
         ret (if all
               (conj ret all `(merge ~dm ~gmap (some-vals ~(get retsel :suba))))
               ret)
         ret (if excess
               (conj ret excess `(merge (not-empty (apply dissoc ~gmap ~sel))
                                        (some-vals ~(get retsel :subexcess))))
               ret)
         ret (if missing
               (conj ret missing `(merge ~missing (some-vals ~(get retsel :submissing))))
               ret)]
    ret))

(defn destructure
  "The bindings of let* for destructuring bindings."
  [bindings]
  (let* [bents (partition 2 bindings)
         pb (fn* pb [bvec b v]
              (cond
                (symbol? b) (conj bvec b v)
                (vector? b) (destvec* pb bvec b v)
                (map? b) (destmap* pb bvec b v)
                :else (throw (argument-error (str "Unsupported binding form: " (pr-str b))))))
         process-entry (fn* [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (reduce process-entry [] bents))))

(defmacro let
  "binding => binding-form init-expr. Evaluates the exprs in a lexical
  context in which the symbols in the binding-forms are bound."
  [bindings & body]
  (check-args &form (vector? bindings) "a vector for its binding"
              (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defn- maybe-destructured [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params new-params [] lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam) (conj lets (first params) gparam))))
        (list new-params `(let ~lets ~@body))))))

(defmacro fn
  "(fn name? [params*] exprs*) and (fn name? ([params*] exprs*)+), with
  destructuring."
  [& sigs]
  (let [name (when (symbol? (first sigs)) (first sigs))
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs)) (list sigs) sigs)
        psig (fn [sig]
               (let [params (first sig)
                     body (rest sig)
                     ;; {:pre [..] :post [..]} conditions: a map before
                     ;; the body, or the metadata of params.
                     conds (when (and (map? (first body)) (next body)) (first body))
                     body (if conds (next body) body)
                     conds (or conds (meta params))
                     pre (get conds :pre)
                     post (get conds :post)
                     body (if post
                            `((let [~'% ~(if (< 1 (count body)) `(do ~@body) (first body))]
                                ~@(map (fn* [c] `(assert ~c)) post)
                                ~'%))
                            body)
                     body (if pre (concat (map (fn* [c] `(assert ~c)) pre) body) body)]
                 (maybe-destructured params body)))
        new-sigs (map psig sigs)]
    ;; The form's metadata goes to fn*, which gives it to the fn.
    (with-meta
      (if name
        (list* 'fn* name new-sigs)
        (cons 'fn* new-sigs))
      (meta &form))))

(defmacro letfn
  "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+);
  the fns may destructure their parameters."
  [fnspecs & body]
  `(letfn* ~(vec (mapcat (fn [spec] [(first spec) (cons `fn spec)]) fnspecs))
           ~@body))

(defn- take-nth-2 [c] (loop* [s (seq c) acc nil] (if s (recur (nnext s) (cons (first s) acc)) (reverse acc))))

(defmacro loop
  "Like let, but the bindings are the target of recur."
  [bindings & body]
  (check-args &form (vector? bindings) "a vector for its binding"
              (even? (count bindings)) "an even number of forms in binding vector")
  (let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (let [vs (take-nth-2 (drop 1 bindings))
            bs (take-nth-2 bindings)
            gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
            bfs (reduce (fn [ret bvg]
                          (let [b (first bvg) v (second bvg) g (nth bvg 2)]
                            (if (symbol? b)
                              (conj ret g v)
                              (conj ret g v b g))))
                        [] (map vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
             (let ~(vec (interleave bs gs))
               ~@body)))))))


(defn- sigs->fdecl
  "Strip doc string and attribute maps from a defn body."
  [fdecl]
  (let [fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
        fdecl (if (map? (first fdecl)) (next fdecl) fdecl)
        fdecl (if (vector? (first fdecl)) (list fdecl) fdecl)
        fdecl (if (map? (last fdecl)) (butlast fdecl) fdecl)]
    fdecl))

(defmacro defn
  "Same as (def name (fn [params* ] exprs*)) or (def name (fn ([params*]
  exprs*)+)) with any doc-string or attrs added to the var metadata."
  [name & fdecl]
  (check-args &form (symbol? name) "a symbol as its name")
  (let [m (if (string? (first fdecl)) {:doc (first fdecl)} {})
        fdecl (if (string? (first fdecl)) (next fdecl) fdecl)
        m (if (map? (first fdecl)) (merge m (first fdecl)) m)
        fdecl (if (map? (first fdecl)) (next fdecl) fdecl)
        fdecl (if (vector? (first fdecl)) (list fdecl) fdecl)
        ;; a trailing attr-map (multi-arity form) is metadata too
        m (if (map? (last fdecl)) (merge m (last fdecl)) m)
        fdecl (if (map? (last fdecl)) (butlast fdecl) fdecl)
        m (merge (meta name) (assoc m :arglists (list 'quote (map first fdecl))))]
    `(def ~(with-meta name m) (fn ~(with-meta name {:display-only true}) ~@fdecl))))

(defmacro defn- [name & decls]
  `(defn ~(vary-meta name assoc :private true) ~@decls))

(defmacro defmacro*
  "defmacro with destructuring."
  [name & body]
  (let [body (sigs->fdecl body)
        add-env (fn [arity] (cons (vec (concat '[&form &env] (first arity))) (rest arity)))]
    `(def ~(with-meta name {:macro true})
       (fn ~(with-meta name {:display-only true}) ~@(map add-env body)))))

;; From here on macros may destructure their arguments.
(defmacro* defmacro [name & body]
  (let [m (if (string? (first body)) {:doc (first body)} {})
        m (let [b (if (string? (first body)) (next body) body)]
            (if (map? (first b)) (merge m (first b)) m))
        body (sigs->fdecl body)
        add-env (fn [arity] (cons (vec (concat '[&form &env] (first arity))) (rest arity)))
        m (merge (meta name) (assoc m :macro true :arglists (list 'quote (map first body))))]
    `(def ~(with-meta name m)
       (fn ~(with-meta name {:display-only true}) ~@(map add-env body)))))

;;; Control flow

(defmacro if-let
  ([bindings then] `(if-let ~bindings ~then nil))
  ([bindings then else]
   (check-args &form (vector? bindings) "a vector for its binding"
               (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (first bindings) tst (second bindings)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#] ~then)
          ~else)))))

(defmacro when-let [bindings & body]
  (let [form (first bindings) tst (second bindings)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#] ~@body)))))

(defmacro if-some
  ([bindings then] `(if-some ~bindings ~then nil))
  ([bindings then else]
   (let [form (first bindings) tst (second bindings)]
     `(let [temp# ~tst]
        (if (nil? temp#)
          ~else
          (let [~form temp#] ~then))))))

(defmacro when-some [bindings & body]
  (let [form (first bindings) tst (second bindings)]
    `(let [temp# ~tst]
       (if (nil? temp#)
         nil
         (let [~form temp#] ~@body)))))

(defmacro when-first [bindings & body]
  (let [x (first bindings) xs (second bindings)]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)] ~@body))))

(defmacro condp [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [n (if (= :>> (second args)) 3 2)
                     a (first args) b (second args) c (nth args 2 nil)
                     more (drop n args)]
                 (if (= 0 (count args))
                   `(throw (k/string-concat "No matching clause: " (k/pr-str ~expr)))
                   (if (= 1 (count args))
                     a
                     (if (= 2 n)
                       `(if (~pred ~a ~expr) ~b ~(emit pred expr more))
                       `(if-let [p# (~pred ~a ~expr)] (~c p#) ~(emit pred expr more)))))))]
    `(let [~gpred ~pred ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

(defmacro case
  "Takes an expression and a set of clauses. Constants are compared with
  k/case-equal (identity, numbers, strings, symbols), collections
  with =."
  [e & clauses]
  (let [ge (gensym "case__")
        default (if (odd? (count clauses))
                  (last clauses)
                  `(throw (corten.lang/ArgumentError. (k/string-concat "No matching clause: "
                                                     (k/pr-str ~ge)))))
        pairs (partition 2 clauses)
        consts (mapcat (fn [p] (let [c (first p)] (if (seq? c) c [c]))) pairs)
        _ (reduce (fn [seen c]
                    (if (contains? seen c)
                      (throw (argument-error (str "Duplicate case test constant: " (pr-str c))))
                      (conj seen c)))
                  #{} consts)
        test1 (fn [x]
                (if (coll? x)
                  `(clojure.core/= ~ge '~x)
                  `(k/case-equal ~ge '~x)))
        test (fn [c]
               (if (seq? c)
                 `(or ~@(map test1 c))
                 (test1 c)))]
    `(let [~ge ~e]
       (cond ~@(mapcat (fn [p] (list (test (first p)) (second p))) pairs)
             :else ~default))))

(defmacro cond-> [expr & clauses]
  (let [g (gensym)
        steps (map (fn [p] (let [test (first p) step (second p)]
                             `(if ~test (-> ~g ~step) ~g)))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat (count steps) g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro cond->> [expr & clauses]
  (let [g (gensym)
        steps (map (fn [p] (let [test (first p) step (second p)]
                             `(if ~test (->> ~g ~step) ~g)))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat (count steps) g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro as-> [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat (count forms) name) (butlast forms))]
     ~(if (empty? forms) name (last forms))))

(defmacro some-> [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step))) forms)]
    `(let [~g ~expr
           ~@(interleave (repeat (count steps) g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro some->> [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (->> ~g ~step))) forms)]
    `(let [~g ~expr
           ~@(interleave (repeat (count steps) g) (butlast steps))]
       ~(if (empty? steps) g (last steps)))))

(defmacro doto [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~@(map (fn [f]
                (if (seq? f)
                  `(~(first f) ~gx ~@(next f))
                  `(~f ~gx)))
              forms)
       ~gx)))

(defmacro ..
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro while [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defmacro dotimes [bindings & body]
  (let [i (first bindings) n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with bindings
  and filtering as provided by \"for\". Does not chunk."
  [seq-exprs & body]
  (check-args &form (vector? seq-exprs) "a vector for its binding"
              (even? (count seq-exprs)) "an even number of forms in binding vector")
  ;; Each step's form is false when the innermost loop around it must
  ;; stop (a :while test failed), true otherwise.
  (let [step (fn step [exprs]
               (if (empty? exprs)
                 `(do ~@body true)
                 (let [k (first exprs) v (second exprs) more (nnext exprs)]
                   (cond
                     (= k :let) `(let ~v ~(step more))
                     (= k :when) `(if ~v ~(step more) true)
                     (= k :while) `(if ~v ~(step more) false)
                     :else (let [gs (gensym "seq__")]
                             `(loop [~gs (seq ~v)]
                                (if ~gs
                                  (if (let [~k (first ~gs)]
                                        ~(step more))
                                    (recur (next ~gs))
                                    true)
                                  true)))))))]
    `(do ~(step (seq seq-exprs)) nil)))

(defmacro for
  "List comprehension (lazy, not chunked)."
  [seq-exprs body-expr]
  (check-args &form (vector? seq-exprs) "a vector for its binding"
              (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [groups (reduce (fn [groups kv]
                         (if (keyword? (first kv))
                           (conj (vec (butlast groups))
                                 (conj (last groups) (first kv) (second kv)))
                           (conj groups [(first kv) (second kv)])))
                       [] (partition 2 seq-exprs))
        emit (fn emit [groups]
               (let [group (first groups)
                     bind (first group) expr (second group) mods (partition 2 (nnext group))
                     giter (gensym "iter__")
                     gxs (gensym "s__")
                     inner (if (next groups)
                             (fn [] `(let [iterys# ~(emit (next groups))
                                           fs# (seq (iterys# ~(second (second groups))))]
                                       (if fs#
                                         (concat fs# (~giter (rest ~gxs)))
                                         (recur (rest ~gxs)))))
                             (fn [] `(cons ~body-expr (~giter (rest ~gxs)))))
                     do-mods (fn do-mods [mods]
                               (if (empty? mods)
                                 (inner)
                                 (let [k (first (first mods)) v (second (first mods))]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mods (next mods)))
                                     (= k :while) `(when ~v ~(do-mods (next mods)))
                                     (= k :when) `(if ~v
                                                    ~(do-mods (next mods))
                                                    (recur (rest ~gxs)))))))]
                 `(fn ~giter [~gxs]
                    (lazy-seq
                     (loop [~gxs ~gxs]
                       (when-first [~bind ~gxs]
                         ~(do-mods mods)))))))]
    `(let [iter# ~(emit groups)]
       (iter# ~(second (first groups))))))

(defmacro assert
  ([x] `(when-not ~x (throw (corten.lang/AssertionError. (k/string-concat "Assert failed: " ~(pr-str x))))))
  ([x message] `(when-not ~x (throw (corten.lang/AssertionError. (k/string-concat "Assert failed: " (k/str ~message) "\n" ~(pr-str x)))))))

(defmacro memfn [name & args]
  `(fn [target# ~@args] (. target# (~name ~@args))))

(defmacro defonce [x init]
  `(let* [v# (def ~x)]
     (when-not (k/var-bound? v#)
       (def ~x ~init))))

(defmacro goog-define
  "A compile-time constant: here just a def of the default value."
  [sym default]
  `(def ~sym ~default))

(defmacro binding
  "binding => var-symbol init-expr. Binds the dynamic vars for the
  execution of body (CL special binding)."
  [bindings & body]
  (let [pairs (partition 2 bindings)]
    `(k/progv (k/list ~@(map (fn [p] `(var ~(first p))) pairs))
                (k/list ~@(map second pairs))
                (fn* [] ~@body))))

(defmacro lazy-seq [& body]
  `(new clojure.core/LazySeq nil (fn* [] ~@body) nil nil))

(defmacro delay [& body]
  `(new clojure.core/Delay (fn* [] ~@body) nil))

(defmacro time [expr]
  `(let [start# (k/get-internal-real-time)
         ret# ~expr]
     (prn (str "Elapsed time: "
               (k// (k/- (k/get-internal-real-time) start#) 1000.0)
               " msecs"))
     ret#))

(defmacro with-in-str
  "Evaluates body with *in* bound to a reader of the string s."
  [s & body]
  `(binding [clojure.core/*in* (corten.lang/StringReader. ~s)]
     ~@body))

(defmacro with-out-str [& body]
  `(let [w# (corten.lang/StringWriter.)]
     ;; *print-fn* too, for code that calls it (cljs).
     (binding [clojure.core/*out* w#
               clojure.core/*print-fn* (fn [x#] (.write w# x#))]
       ~@body)
     (str w#)))

(defn- require-forms
  "Forms for one :require spec: lib or [lib :as alias :refer [names]
  :rename {name local} :as-alias alias]. With macros? (:require-macros),
  a namespace requiring its own macros loads its .clj file."
  ([spec] (require-forms spec false))
  ([spec macros?]
  (let [lib (if (vector? spec) (first spec) spec)
        opts (if (vector? spec) (apply hash-map (rest spec)) {})
        alias (get opts :as)
        as-alias (get opts :as-alias)
        refer (get opts :refer)
        refer-macros (get opts :refer-macros)
        rename (get opts :rename {})
        lns (gensym "ns")
        refer-1 (fn [r]
                  (let [local (get rename r)]
                    `(k/refer-name *ns* ~lns ~(name r) ~@(when local [(name local)]))))]
    (if (and as-alias (not alias) (not refer) (not refer-macros))
      ;; :as-alias only: the namespace is not loaded.
      (list `(k/alias *ns* ~(name as-alias) (k/create-ns ~(name lib))))
      (list
       `(let [~lns (~(if macros? 'corten.kernel/require-macros 'corten.kernel/require) ~(name lib))]
          ~@(when alias (list `(k/alias *ns* ~(name alias) ~lns)))
          ~@(when as-alias (list `(k/alias *ns* ~(name as-alias) ~lns)))
          ~@(cond (= refer :all) (list `(k/refer *ns* ~lns))
                  refer (map refer-1 refer)
                  :else nil)
          ~@(map refer-1 refer-macros)))))))

(defn- import-forms
  "Forms for one :import spec [lib Type*]: refer the types."
  [spec]
  (let [lns (gensym "ns")]
    (list
     `(let [~lns (k/require ~(name (first spec)))]
        ~@(map (fn [t] `(k/refer-name *ns* ~lns ~(name t)))
               (rest spec))))))

(defn- use->require [spec]
  (if (vector? spec)
    (let [opts (apply hash-map (rest spec))]
      (vec (concat [(first spec)]
                   (if (contains? opts :only) [:refer (get opts :only)] [:refer :all])
                   (mapcat identity (dissoc opts :only)))))
    [spec :refer :all]))

(defmacro ns
  "(ns name (:require lib-spec*) (:refer-clojure :exclude [names] :rename {name local}))."
  [ns-name & references]
  (let [refs (filter seq? references)
        rc (some (fn [r] (when (= (first r) :refer-clojure) r)) refs)
        rc-opts (if rc (apply hash-map (rest rc)) {})
        rename (get rc-opts :rename {})
        exclude (concat (get rc-opts :exclude) (keys rename))
        clause (fn [ref]
                 (let [k (first ref) args (rest ref)]
                   (cond
                     (or (= k :require) (= k :require-macros))
                     (mapcat #(require-forms % (= k :require-macros)) args)
                     ;; (:use lib) refers all; (:use [lib :only [names]]).
                     (or (= k :use) (= k :use-macros))
                     (mapcat (fn [spec] (require-forms (use->require spec) (= k :use-macros))) args)
                     ;; (:import [lib Type*]): refer the types.
                     (= k :import)
                     (mapcat import-forms args)
                     :else nil)))]
    `(do (k/in-ns '~ns-name nil)
         (k/refer *ns* (k/find-ns "clojure.core")
                        (k/list ~@(map name exclude)))
         ~@(map (fn [[k v]] `(k/refer-name *ns* (k/find-ns "clojure.core")
                                                  ~(name k) ~(name v)))
                rename)
         ~@(mapcat clause refs)
         nil)))

;;; Types and protocols

(defn- type-ref
  "The expression for type name t in extend-type: kernel types for the
  ClojureScript base type names."
  [t]
  (cond
    (nil? t) `(k/builtin-type "nil")
    (and (symbol? t) (nil? (namespace t))
           (some (fn [n] (= n (name t)))
                 ["nil" "number" "string" "boolean" "array" "function" "object"
                  "default" "char" "Symbol" "Keyword"
                  "Var" "Namespace"]))
    `(k/builtin-type '~t)
    :else t))

(defn- protocol-value [p]
  (let [v (resolve p)]
    (when-not (and v (k/var-bound? v) (k/protocol? (k/var-get v)))
      (throw (argument-error (str "Not a protocol: " (pr-str p)))))
    (k/var-get v)))

(defn- method-id [proto pname mname]
  (or (k/protocol-method-id proto (name mname))
      (throw (argument-error (str "No method " mname " in protocol " pname)))))

(defn- parse-impls
  "Split specs [Proto1 (m [args] body) ... Proto2 ...] into a list of
  [proto [methods]]."
  [specs]
  (loop [s (seq specs) acc [] cur nil]
    (if s
      (let [x (first s)]
        (if (symbol? x)
          (recur (next s) (if cur (conj acc cur) acc) [x []])
          (recur (next s) acc [(first cur) (conj (second cur) x)])))
      (if cur (conj acc cur) acc))))

(defn- group-methods
  "Methods by name: ((name ([params] body) ...) ...), merging separate
  arities and the (name ([params] body)+) form."
  [methods]
  (let [arities (fn [m] (if (vector? (second m)) (list (rest m)) (rest m)))]
    (reduce (fn [acc m]
              (let [n (first m)
                    found (some (fn [e] (when (= (first e) n) e)) acc)]
                (if found
                  (map (fn [e] (if (= (first e) n) (concat e (arities m)) e)) acc)
                  (concat acc (list (cons n (arities m)))))))
            nil methods)))

(defn- recur-arity?
  "Does a recur with n arguments occur in forms (outside nested fns and
  loops)?"
  [forms n]
  (some (fn [x]
          (when (seq? x)
            (let [h (first x)]
              (cond (= h 'recur) (= (count (rest x)) n)
                    (contains? '#{fn fn* loop loop* quote reify deftype letfn} h) false
                    :else (recur-arity? x n)))))
        forms))

(defn- method-fn
  "fn for a method; with fields, they are locals (fields* on this).
  IFn's -invoke of a type named tname is named tname in arity errors."
  [mname arities fields tname]
  (let [arity (fn [a]
                (let [params (first a)
                      ;; IFn's last -invoke arity [this a..t rest]: rest
                      ;; gets the arguments past 20, as a seq.
                      params (if (and (= (name mname) "-invoke") (= (count params) 22))
                               (vec (concat (butlast params) ['& (last params)]))
                               params)
                      body (rest a)
                      ;; recur in a method body does not pass this
                      ;; (cljs also accepts it, with a warning).
                      args (if (recur-arity? body (count params)) params (rest params))
                      body (if (and (every? symbol? args) (not (some (fn [x] (= x '&)) args)))
                             (list `(loop* ~(vec (interleave args args)) ~@body))
                             body)
                      ;; A destructured this: [[x]].
                      this (first params)
                      dthis? (not (symbol? this))
                      params (if dthis? (vec (cons (gensym "this__") (rest params))) params)
                      body (if dthis? (list `(let [~this ~(first params)] ~@body)) body)]
                  (if fields
                    (let [this (first params)
                          this* (if (= (name this) "_") (gensym "this__") this)
                          params (vec (cons this* (rest params)))
                          ;; Parameters shadow fields of the same name.
                          pnames (set (map name (filter symbol? params)))
                          fields (vec (map (fn [f] (if (contains? pnames (name f)) (gensym "shadowed") f))
                                           fields))]
                      (list params `(fields* ~this* ~fields ~@body)))
                    (cons params body))))]
    ;; Not named after the method: the body must see the protocol fn.
    `(fn ~(if (and tname (= (name mname) "-invoke"))
            (with-meta 'invoke__method {:display-only tname})
            (symbol (str (name mname) "__method")))
       ~@(map arity arities))))

(defn- emit-impls [tsym impls fields tname]
  (mapcat
   (fn [impl]
     (let [p (first impl) methods (group-methods (second impl))]
       (if (= p 'Object)
         (map (fn [m]
                `(k/add-obj-method ~tsym ~(name (first m)) ~(method-fn (first m) (rest m) fields tname)))
              methods)
         (let [proto (protocol-value p)]
           (cons
            `(k/extend ~tsym ~(k/protocol-marker proto) true)
            (map (fn [m]
                   `(k/extend ~tsym ~(method-id proto p (first m))
                                    ~(method-fn (first m) (rest m) fields tname)))
                 methods))))))
   (parse-impls impls)))

(defmacro defprotocol
  "(defprotocol P doc? (method [this args*]+ doc?)*)"
  [psym & doc+methods]
  (let [methods (filter seq? doc+methods)
        opts (loop [xs doc+methods opts {}]
               (cond (not (seq xs)) opts
                     (keyword? (first xs)) (recur (nnext xs) (assoc opts (first xs) (second xs)))
                     :else (recur (next xs) opts)))
        via-meta (get opts :extend-via-metadata)
        pname (str (k/ns-name *ns*) "/" (name psym))
        mdef (fn [m]
               (let [mname (first m)
                     sigs (filter vector? (rest m))
                     gid (gensym "id")
                     mf (gensym "f")
                     msym (symbol (k/ns-name *ns*) (name mname))
                     call (fn [ps]
                            (let [c `((k/method ~gid ~(first ps)) ~@ps)]
                              (if via-meta
                                ;; An implementation in the metadata comes first.
                                `(let* [~mf (clojure.core/get (clojure.core/meta ~(first ps)) '~msym)]
                                   (if ~mf (~mf ~@ps) ~c))
                                c)))]
                 `(def ~mname
                    (let* [~gid (k/protocol-method-id ~psym ~(name mname))]
                      (fn* ~mname
                           ~@(map (fn [sig]
                                    (let [ps (vec (map (fn [i] (gensym "p")) sig))]
                                      `(~ps ~(call ps))))
                                  sigs))))))]
    `(do
       (def ~psym (k/make-protocol ~pname (k/list ~@(map (fn [m] (name (first m))) methods))))
       ~@(map mdef methods)
       '~psym)))

(defmacro deftype
  "(deftype Name [fields*] Protocol (method [this args*] body)* ...
  Object (method ...)*)"
  [tsym fields & impls]
  (let [tname (str (k/ns-name *ns*) "/" (name tsym))
        ;; cljs.core's Symbol and Keyword and corten.lang's error types
        ;; extend the kernel types (which have the same fields).
        kernel? (k/kernel-type tname)]
    `(do
       (def ~tsym ~(if kernel?
                     `(k/kernel-type ~tname)
                     `(k/make-type ~tname (k/list ~@(map name fields)))))
       ~@(emit-impls tsym impls fields tname)
       ~@(when-not kernel? [`(set! (.-getBasis ~tsym) (fn [] '~(vec fields)))])
       (def ~(with-meta (symbol (str "->" (name tsym)))
               {:doc (str "Positional factory function for " (k/ns-name *ns*) "/" (name tsym) ".")})
         (fn* [~@fields] (new ~tsym ~@fields)))
       ~tsym)))

(defn- emit-defrecord
  "The protocol implementations of a record (from cljs/core.cljc)."
  [tsym base-fields]
  (let [rname (str (k/ns-name *ns*) "." (name tsym))
        fields (concat base-fields '[__meta __extmap __hash])
        kws (map keyword base-fields)
        ;; fields with field f replaced by expression x
        fields-with (fn [f x] (map (fn [g] (if (= g f) x g)) fields))
        new-with (fn [f x] `(new ~tsym ~@(map (fn [g] (if (= g '__hash) nil g)) (fields-with f x))))
        this (gensym "this") other (gensym "other") k (gensym "k") v (gensym "v")]
    ['IRecord
     'ICloneable
     `(~'-clone [~this] (new ~tsym ~@fields))
     'IHash
     `(~'-hash [~this]
       (caching-hash ~this
                     (fn [coll#] (bit-xor (hash ~rname) (hash-unordered-coll coll#)))
                     ~'__hash))
     'IEquiv
     `(~'-equiv [~this ~other]
       (and (some? ~other)
            (identical? (k/type-of ~this) (k/type-of ~other))
            ~@(map (fn [f] `(= ~f (. ~other ~(symbol (str "-" f))))) base-fields)
            (= ~'__extmap (.-__extmap ~other))))
     'IMeta
     `(~'-meta [~this] ~'__meta)
     'IWithMeta
     `(~'-with-meta [~this ~v] (new ~tsym ~@(fields-with '__meta v)))
     'ILookup
     `(~'-lookup [~this ~k] (-lookup ~this ~k nil))
     `(~'-lookup [~this ~k else#]
       (case ~k
         ~@(mapcat (fn [f] [(keyword f) f]) base-fields)
         (get ~'__extmap ~k else#)))
     'ICounted
     `(~'-count [~this] (+ ~(count base-fields) (count ~'__extmap)))
     'ICollection
     `(~'-conj [~this entry#]
       (if (map-pair? entry#)
         (-assoc ~this (-nth entry# 0) (-nth entry# 1))
         (reduce -conj ~this entry#)))
     'IAssociative
     `(~'-contains-key? [~this ~k]
       ~(if (seq base-fields)
          `(case ~k (~@kws) true (contains? ~'__extmap ~k))
          `(contains? ~'__extmap ~k)))
     `(~'-assoc [~this ~k ~v]
       (condp keyword-identical? ~k
         ~@(mapcat (fn [f] [(keyword f) (new-with f v)]) base-fields)
         (new ~tsym ~@base-fields ~'__meta (assoc ~'__extmap ~k ~v) nil)))
     'IMap
     `(~'-dissoc [~this ~k]
       (if (contains? #{~@kws} ~k)
         (dissoc (-with-meta (into {} ~this) ~'__meta) ~k)
         (new ~tsym ~@base-fields ~'__meta (not-empty (dissoc ~'__extmap ~k)) nil)))
     'ISeqable
     `(~'-seq [~this]
       (seq (concat [~@(map (fn [f] `(MapEntry. ~(keyword f) ~f nil)) base-fields)]
                    ~'__extmap)))
     'IIterable
     `(~'-iterator [~this]
       (RecordIter. 0 ~this ~(count base-fields) [~@kws]
                    (if ~'__extmap (-iterator ~'__extmap) (nil-iter))))
     'IPrintWithWriter
     `(~'-pr-writer [~this writer# opts#]
       ;; PORT: pr-sequential-writer passes writer and opts too.
       (let [pr-pair# (fn [keyval# _# _#] (pr-sequential-writer writer# pr-writer "" " " "" opts# keyval#))]
         (pr-sequential-writer
          writer# pr-pair# ~(str "#" rname "{") ", " "}" opts#
          (concat [~@(map (fn [f] `(vector ~(keyword f) ~f)) base-fields)]
                  ~'__extmap))))
     'IKVReduce
     `(~'-kv-reduce [~this f# init#]
       (reduce (fn [ret# e#] (f# ret# (-nth e# 0) (-nth e# 1))) init# ~this))]))

(defmacro defrecord
  "(defrecord name [fields*] specs*): a deftype with the extra fields
  __meta __extmap __hash and map behavior, plus ->name and map->name."
  [tsym fields & impls]
  (let [all-fields (vec (concat fields ['__meta '__extmap (with-meta '__hash {:mutable true})]))
        ;; ^:mutable stays on the deftype fields only.
        fields (vec (map (fn [f] (with-meta f nil)) fields))
        ms (gensym "m")]
    `(do
       (deftype ~tsym ~all-fields ~@impls ~@(emit-defrecord tsym fields))
       ;; (Name. a b) leaves __meta, __extmap and __hash nil.
       (set! (.-optionalFields ~tsym) true)
       (set! (.-getBasis ~tsym) (fn [] '~fields))
       (def ~(with-meta (symbol (str "->" (name tsym)))
               {:doc (str "Positional factory function for " (k/ns-name *ns*) "/" (name tsym) ".")})
         (fn* [~@fields] (new ~tsym ~@fields nil nil nil)))
       (def ~(with-meta (symbol (str "map->" (name tsym)))
               {:doc (str "Factory function for " (k/ns-name *ns*) "/" (name tsym)
                          ", taking a map of keywords to field values.")})
         (fn* [~ms]
           (let [extmap# (cond->> (dissoc ~ms ~@(map keyword fields))
                           (record? ~ms) (into {}))]
             (new ~tsym ~@(map (fn [f] `(get ~ms ~(keyword f))) fields)
                  nil (not-empty extmap#) nil))))
       ~tsym)))

(defmacro extend-type [t & impls]
  (let [g (gensym "type__")]
    `(let [~g ~(type-ref t)]
       ~@(emit-impls g impls nil (str t))
       nil)))

(defmacro specify!
  "Extends the object x (an instance) with the protocol implementations;
  x gets a type of its own. Returns x."
  [expr & impls]
  (let [x (gensym "x__") g (gensym "type__")]
    `(let [~x ~expr ~g (k/specify ~x)]
       ~@(emit-impls g impls nil nil)
       ~x)))

(defmacro specify
  "Like specify!, on a clone of expr."
  [expr & impls]
  `(specify! (clone ~expr) ~@impls))

(defmacro extend-protocol [p & specs]
  (let [groups (loop [s (seq specs) acc [] cur nil]
                 (if s
                   (if (seq? (first s))
                     (recur (next s) acc (conj cur (first s)))
                     (recur (next s) (if cur (conj acc cur) acc) [(first s)]))
                   (if cur (conj acc cur) acc)))]
    `(do ~@(map (fn [g] `(extend-type ~(first g) ~p ~@(rest g))) groups)
         nil)))

(defmacro reify
  "An instance of a new anonymous type. Each method's fn is stored in a
  field, so methods close over the lexical environment. A last field
  holds the metadata (IMeta and IWithMeta, unless implemented)."
  [& impls]
  (let [entries (mapcat (fn [impl]
                          (map (fn [m] [(first impl) (first m) (rest m)])
                               (group-methods (second impl))))
                        (parse-impls impls))
        n (count entries)
        ty (k/make-type "reify" (concat (map (fn [i] (str "m" i)) (range n)) (list "meta")))
        protos (set (map first entries))]
    (when-not (or (contains? protos 'IMeta) (contains? protos 'IWithMeta))
      (let [imeta (protocol-value 'IMeta) iwm (protocol-value 'IWithMeta)]
        (k/extend ty (k/protocol-marker imeta) true)
        (k/extend ty (method-id imeta 'IMeta '-meta) (fn [this] (k/field this n)))
        (k/extend ty (k/protocol-marker iwm) true)
        (k/extend ty (method-id iwm 'IWithMeta '-with-meta)
                        (fn [this m] (k/with-field this n m)))))
    ;; Install on the anonymous type: each method calls the fn in its field.
    (loop [es entries i 0]
      (when es
        (let [e (first es) p (first e) m (second e)
              f (fn [this & args] (k/apply (k/field this i) this args))]
          (if (= p 'Object)
            (k/add-obj-method ty (name m) f)
            (let [proto (protocol-value p)]
              (k/extend ty (k/protocol-marker proto) true)
              (k/extend ty (method-id proto p m) f))))
        (recur (next es) (inc i))))
    `(k/new '~ty ~@(map (fn [e] (method-fn (second e) (nth e 2) nil nil)) entries) nil)))

(defmacro satisfies? [p x]
  `(k/has-method (k/protocol-marker ~p) ~x))

(defmacro implements? [p x]
  `(k/implements (k/protocol-marker ~p) ~x))

(defmacro instance? [t x]
  ;; The type itself, else a subtype (exception classes).
  `(let [x# ~x]
     (if (k/eq (k/type-of x#) ~(type-ref t))
       true
       (k/instance? ~(type-ref t) x#))))

;;; Inline versions of core functions (direct calls only; core.cljs
;;; defines the functions, often in terms of these).

(defmacro nil? [x] `(k/eq ~x nil))
(defmacro some? [x] `(if (k/eq ~x nil) false true))
(defmacro true? [x] `(k/eq ~x true))
(defmacro false? [x] `(k/eq ~x false))
;; PORT: as JavaScript's ===, equal strings and numbers are identical
;; (cljs.core compares symbol names with identical?).
(defmacro identical? [a b]
  `(let [a# ~a b# ~b]
     (if (k/eq a# b#) true (k/identical? a# b#))))
(defmacro not [x] `(if ~x false true))
(defmacro number? [x] `(k/number? ~x))
(defmacro string? [x] `(k/string? ~x))
(defmacro char? [x] `(k/char? ~x))
(defmacro symbol? [x] `(k/symbol? ~x))
(defmacro keyword? [x] `(k/keyword? ~x))
(defmacro array? [x] `(k/array? ~x))
(defmacro boolean [x] `(if ~x true false))

(defmacro +
  ([] 0)
  ([x] x)
  ([x y] `(k/+ ~x ~y))
  ([x y & more] `(+ (+ ~x ~y) ~@more)))
;; (-) and (/) call the function, which throws (Clojure's :inline-arities).
(defmacro -
  ([] `((var -)))
  ([x] `(k/- ~x))
  ([x y] `(k/- ~x ~y))
  ([x y & more] `(- (- ~x ~y) ~@more)))
(defmacro *
  ([] 1)
  ([x] x)
  ([x y] `(k/* ~x ~y))
  ([x y & more] `(* (* ~x ~y) ~@more)))
;; Clojure division: ratios; exact division by zero is an error.
(defmacro /
  ([] `((var /)))
  ([x] `(k/div 1 ~x))
  ([x y] `(k/div ~x ~y))
  ([x y & more] `(/ (/ ~x ~y) ~@more)))
(defmacro divide [x y] `(k/div ~x ~y))
(defmacro inc [x] `(k/+ ~x 1))
(defmacro dec [x] `(k/- ~x 1))

(defn- compare-op [op]
  (fn
    ([x] true)
    ([x y] `(~op ~x ~y))
    ([x y & more] `(~op ~x ~y ~@more))))
(defmacro < [& args] (apply (compare-op 'corten.kernel/<) args))
(defmacro > [& args] (apply (compare-op 'corten.kernel/>) args))
(defmacro <= [& args] (apply (compare-op 'corten.kernel/<=) args))
(defmacro >= [& args] (apply (compare-op 'corten.kernel/>=) args))
(defmacro == [& args] (apply (compare-op 'corten.kernel/=) args))
;; True for 0, 0.0 and -0.0; false for non-numbers.
;; True for 0, 0.0 and -0.0; false for non-numbers.
(defmacro zero? [x] `(k/zero? ~x))
(defmacro pos? [x] `(k/> ~x 0))
(defmacro neg? [x] `(k/< ~x 0))
;; NaN if either argument is NaN.
(defmacro max ([x] x) ([x y] `(k/max ~x ~y)) ([x y & more] `(max (max ~x ~y) ~@more)))
(defmacro min ([x] x) ([x y] `(k/min ~x ~y)) ([x y & more] `(min (min ~x ~y) ~@more)))
;; Doubles if either argument is a float.
(defmacro quot [n d] `(k/quot ~n ~d))
(defmacro rem [n d] `(k/rem ~n ~d))

;; Bit operations on Longs (64 bits; shift counts mod 64), as in Clojure.
(defmacro bit-and ([x y] `(k/lbit 0 ~x ~y)) ([x y & more] `(bit-and (bit-and ~x ~y) ~@more)))
(defmacro bit-or ([x y] `(k/lbit 1 ~x ~y)) ([x y & more] `(bit-or (bit-or ~x ~y) ~@more)))
(defmacro bit-xor ([x y] `(k/lbit 2 ~x ~y)) ([x y & more] `(bit-xor (bit-xor ~x ~y) ~@more)))
(defmacro bit-and-not ([x y] `(k/lbit 3 ~x ~y)) ([x y & more] `(bit-and-not (bit-and-not ~x ~y) ~@more)))
(defmacro bit-not [x] `(k/lbit-not ~x))
(defmacro bit-shift-left [x n] `(k/lbit 4 ~x ~n))
(defmacro bit-shift-right [x n] `(k/lbit 5 ~x ~n))
(defmacro unsigned-bit-shift-right [x n] `(k/lbit 6 ~x ~n))
(defmacro bit-shift-right-zero-fill [x n] `(k/lbit 6 ~x ~n))
(defmacro bit-clear [x n] `(k/lbit 3 ~x (k/lbit 4 1 ~n)))
(defmacro bit-set [x n] `(k/lbit 1 ~x (k/lbit 4 1 ~n)))
(defmacro bit-flip [x n] `(k/lbit 2 ~x (k/lbit 4 1 ~n)))
(defmacro bit-test [x n] `(k/not (k/= 0 (k/lbit 0 ~x (k/lbit 4 1 ~n)))))
;; 32-bit int operations for the hash functions (JavaScript semantics).
(defmacro int-bit-and [x y] `(k/bit 0 ~x ~y))
(defmacro int-bit-or [x y] `(k/bit 1 ~x ~y))
(defmacro int-bit-xor [x y] `(k/bit 2 ~x ~y))
(defmacro int-shift-left [x n] `(k/bit 4 ~x ~n))
(defmacro int-shift-right [x n] `(k/bit 5 ~x ~n))
(defmacro int-ushift-right [x n] `(k/bit 6 ~x ~n))
(defmacro imul [a b] `(k/bit 7 ~a ~b))
;; Conversions (src/clj/number.rs).
(defmacro int [x] `(k/num-cast 1 ~x))
(defmacro long [x] `(k/num-cast 0 ~x))
(defmacro unchecked-int [x] `(k/num-cast 4 ~x))
(defmacro unchecked-long [x] `(k/num-cast 5 ~x))
(defmacro double [x] `(k/num-cast 10 ~x))
(defmacro +' ([] 0) ([x] x) ([x y] `(k/arith 0 1 ~x ~y)) ([x y & more] `(+' (+' ~x ~y) ~@more)))
(defmacro -' ([x] `(k/arith 1 1 0 ~x)) ([x y] `(k/arith 1 1 ~x ~y)) ([x y & more] `(-' (-' ~x ~y) ~@more)))
(defmacro *' ([] 1) ([x] x) ([x y] `(k/arith 2 1 ~x ~y)) ([x y & more] `(*' (*' ~x ~y) ~@more)))
(defmacro inc' [x] `(k/arith 0 1 ~x 1))
(defmacro dec' [x] `(k/arith 1 1 ~x 1))
(defmacro unchecked-add ([] 0) ([x] x) ([x y] `(k/arith 0 2 ~x ~y)) ([x y & more] `(unchecked-add (unchecked-add ~x ~y) ~@more)))
(defmacro unchecked-subtract ([x] `(k/arith 1 2 0 ~x)) ([x y] `(k/arith 1 2 ~x ~y)) ([x y & more] `(unchecked-subtract (unchecked-subtract ~x ~y) ~@more)))
(defmacro unchecked-multiply ([] 1) ([x] x) ([x y] `(k/arith 2 2 ~x ~y)) ([x y & more] `(unchecked-multiply (unchecked-multiply ~x ~y) ~@more)))
(defmacro unchecked-inc [x] `(k/arith 0 2 ~x 1))
(defmacro unchecked-dec [x] `(k/arith 1 2 ~x 1))
(defmacro unchecked-negate [x] `(k/arith 1 2 0 ~x))
(defmacro unchecked-add-int [x y] `(k/num-cast 4 (k/arith 0 2 ~x ~y)))
(defmacro unchecked-subtract-int ([x] `(k/num-cast 4 (k/arith 1 2 0 ~x))) ([x y] `(k/num-cast 4 (k/arith 1 2 ~x ~y))))
(defmacro unchecked-multiply-int [x y] `(k/num-cast 4 (k/arith 2 2 ~x ~y)))
(defmacro unchecked-inc-int [x] `(k/num-cast 4 (k/arith 0 2 ~x 1)))
(defmacro unchecked-dec-int [x] `(k/num-cast 4 (k/arith 1 2 ~x 1)))
(defmacro unchecked-negate-int [x] `(k/num-cast 4 (k/arith 1 2 0 ~x)))
(defmacro unchecked-remainder-int [x y] `(k/rem ~x ~y))
(defmacro unchecked-divide-int [x y] `(k/quot ~x ~y))

;;; Arrays (src/clj/array.rs).

(defmacro aget
  ([a i] `(k/aget ~a ~i))
  ([a i & idxs] `(aget (aget ~a ~i) ~@idxs)))
(defmacro aset
  ([a i v] `(k/aset ~a ~i ~v))
  ([a i j & more] `(aset (aget ~a ~i) ~j ~@more)))
(defmacro alength [a] `(k/alength ~a))
(defmacro array [& xs] `(k/array ~@xs))
(defmacro make-array
  ([size] `(k/make-array ~size))
  ([type size] `(k/make-array ~size))
  ([type size & more] `((var make-array) ~type ~size ~@more)))

;;; JavaScript-specific macros of ClojureScript used by cljs.core.

(defmacro exists?
  "True if x is defined: for a symbol, if it resolves to a bound var
  (for ns/name, one interned in ns) or names a loaded namespace (in
  JavaScript, its object)."
  [x]
  (if (symbol? x)
    (let [v (resolve x)
          home (when (namespace x) (k/resolve-ns (namespace x)))]
      (if (or (and v (k/var-bound? v) (or (nil? (namespace x)) (k/eq (k/var-ns v) home)))
              (and (nil? (namespace x)) (k/find-ns (name x))))
        true false))
    `(some? ~x)))
;; JavaScript %: truncating, a double if either argument is a float.
(defmacro js-mod [n d] `(k/rem ~n ~d))
(defmacro js-fn? [x] `(k/js-fn? ~x))
(defmacro js-iterable? [x] `(do ~x false))
(defmacro js-symbol? [x] `(do ~x false))
(defmacro js-keys [o] `(k/okeys ~o))
(defmacro js-delete [o k] `(k/odelete ~o ~k))
(defmacro js-obj [& kvs] `(k/js-obj ~@kvs))
(defmacro coercive-= [a b] `(identical? ~a ~b))
(defmacro unchecked-get [o k] `(k/oget ~o ~k))
(defmacro unchecked-set [o k v] `(k/oset ~o ~k ~v))
(defmacro unchecked-max [a b] `(k/max ~a ~b))
(defmacro unchecked-min [a b] `(k/min ~a ~b))
(defmacro when-assert [& body] `(when *assert* ~@body))
(defmacro es6-iterable [ty] nil)

(defmacro caching-hash [coll hash-fn hash-key]
  `(let [h# ~hash-key]
     (if-not (nil? h#)
       h#
       (let [h# (~hash-fn ~coll)]
         (set! ~hash-key h#)
         h#))))

(defmacro amap [a idx ret expr]
  `(let [a# ~a
         l# (alength a#)
         ~ret (aclone a#)]
     (loop [~idx 0]
       (if (< ~idx l#)
         (do (aset ~ret ~idx ~expr)
             (recur (inc ~idx)))
         ~ret))))

(defmacro areduce [a idx ret init expr]
  `(let [a# ~a l# (alength a#)]
     (loop [~idx 0 ~ret ~init]
       (if (< ~idx l#)
         (recur (inc ~idx) ~expr)
         ~ret))))

(defmacro vswap! [vol f & args]
  `(let [v# ~vol] (-vreset! v# (~f (-deref v#) ~@args))))
(defmacro truth_ [x] `(if ~x true false))

;;; Multimethods (from cljs/core.cljc).

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  Options: :default (dispatch value, default :default) and :hierarchy."
  [mm-name & options]
  (let [docstring (if (string? (first options)) (first options) nil)
        options (if (string? (first options)) (next options) options)
        m (if (map? (first options)) (first options) {})
        options (if (map? (first options)) (next options) options)
        dispatch-fn (first options)
        options (next options)
        m (if docstring (assoc m :doc docstring) m)
        m (if (meta mm-name) (conj (meta mm-name) m) m)
        mm-ns (k/ns-name *ns*)]
    (when (= (count options) 1)
      (throw (argument-error "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
    (let [options (apply hash-map options)
          default (get options :default :default)]
      `(defonce ~(with-meta mm-name m)
         (let [method-table# (atom {})
               prefer-table# (atom {})
               method-cache# (atom {})
               cached-hierarchy# (atom {})
               hierarchy# (get ~options :hierarchy (clojure.core/get-global-hierarchy))]
           (clojure.core/MultiFn. (symbol ~mm-ns ~(name mm-name)) ~dispatch-fn ~default hierarchy#
                                  method-table# prefer-table# method-cache# cached-hierarchy#))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value."
  [multifn dispatch-val & fn-tail]
  `(clojure.core/-add-method ~multifn ~dispatch-val (fn ~@fn-tail)))

;;; Vars (functions in core-extras.clj).

(defmacro with-bindings
  "Takes a map of Var/value pairs. Installs for the given Vars the
  associated values as thread-local bindings. Then executes body. Pops
  the installed bindings after body was evaluated. Returns the value of
  body."
  [binding-map & body]
  `(with-bindings* ~binding-map (fn [] ~@body)))

(defmacro bound-fn
  "Returns a function defined by the given fntail, which will install
  the same bindings in effect as in the thread at the time bound-fn was
  called."
  [& fntail]
  `(bound-fn* (fn ~@fntail)))

(defmacro with-redefs
  "binding => var-symbol temp-value-expr. Temporarily redefines Vars
  while executing the body. The temp-value-exprs will be evaluated and
  each resulting value will replace in parallel the root value of its
  Var. After the body is executed, the root values of all the Vars will
  be set back to their old values."
  [bindings & body]
  `(with-redefs-fn ~(zipmap (map (fn [s] (list 'var s)) (take-nth 2 bindings))
                            (take-nth 2 (next bindings)))
     (fn [] ~@body)))

;;; The reducers macros (from cljs/core.cljc).

(defn- do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defmacro ^:private defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

(defn- remove-sym
  "PORT: (clojure.walk/postwalk ...) of do-rfn: removes k from every
  sequential form in x."
  [k x]
  (cond
    (vector? x) (vec (remove (fn [y] (= k y)) (map (fn [y] (remove-sym k y)) x)))
    (seq? x) (apply list (remove (fn [y] (= k y)) (map (fn [y] (remove-sym k y)) x)))
    :else x))

(defn- do-rfn [f1 k fkv]
  `(fn
     ([] (~f1))
     ~(remove-sym k fkv)
     ~fkv))

(defmacro ^:private rfn
  "Builds 3-arity reducing fn given names of wrapped fn and key, and k/v impl."
  [[f1 k] fkv]
  (do-rfn f1 k fkv))

;;; selector (Clojure 1.13; from cljs/core.cljc).

(defn- selector-impl [m]
  (let [dirs [:select :excess :missing :all]
        present (filter (fn [d] (contains? m d)) dirs)
        names (zipmap present (map (fn [_] (gensym)) present))]
    (if (empty? names)
      (throw (argument-error "form must contain at least one of :select :excess :missing :all"))
      `(fn ~(gensym "selector")
         [map#]
         (let [~(merge m names) map#]
           ~(if (= 1 (count names))
              (val (first names))
              (list `some-vals names)))))))

(defmacro selector
  "Builds a selecting-fn from m, a map destructuring form that must
  include one or more of the :select, :all, :missing, and :excess
  directives. The return function takes a collection, destructures it
  per m, and returns a map of the result(s).

  If m has exactly one directive, the result is the value that
  directive would yield. If m has more than one directive, then it
  returns a map of directives to values.

  As in destructuring, :missing controls whether missing required keys
  throw or are collected.

  While a map destructuring form may and sometimes must include
  bindings, selector doesn't produce bindings, thus ignoring the
  associated directive names.

  Throws an exception if the argument is not a map."
  {:added "1.13"}
  [m]
  (when-not (map? m)
    (throw (argument-error "expected a map")))
  (selector-impl m))
