;;;; Clojure bootstrap, stage 1: clj.boot, a small eager clojure.core
;;;; for writing the macros of macros.clj before cljs.core exists. It is
;;;; written on the kernel's sequence functions (src/clj/coll.rs), which
;;;; work natively on the collections the reader makes and call
;;;; clojure.core's functions for other ones, so the macros stay correct
;;;; for any collection once cljs.core is loaded. Sequence functions
;;;; return lists. Syntax-quote writes vars of clj.boot as clojure.core/...

(k/create-ns 'clj.boot)
(in-ns 'clj.boot)
(corten.kernel/alias (corten.kernel/find-ns 'clj.boot) 'k (corten.kernel/find-ns 'corten.kernel))

;;; Predicates (Clojure booleans)

(defn boolean [x] (if x true false))
(defn not [x] (if x false true))
(defn nil? [x] (k/eq x nil))
(defn some? [x] (not (nil? x)))
(defn true? [x] (k/eq x true))
(defn false? [x] (k/eq x false))
(defn identical? [a b] (k/eq a b))
(defn symbol? [x] (k/symbol? x))
(defn keyword? [x] (k/keyword? x))
(defn ident? [x] (if (k/symbol? x) true (k/keyword? x)))
(defn string? [x] (k/string? x))
(defn char? [x] (k/char? x))
(defn number? [x] (k/number? x))
(defn integer? [x] (k/integer? x))
(defn fn? [x] (k/fn? x))
(defn vector? [x] (k/vector? x))
(defn map? [x] (k/map? x))
(defn set? [x] (k/set? x))
(defn seq? [x] (k/seq? x))
(defn list? [x] (k/seq? x))
(defn coll? [x] (or (seq? x) (vector? x) (map? x) (set? x)))
(defn sequential? [x] (or (seq? x) (vector? x)))
(defn boolean? [x] (k/boolean? x))

;;; Sequences

(defn seq [x] (k/seq x))
(defn first [x] (k/first x))
(defn rest [x] (k/rest x))
(defn next [x] (k/next x))
(defn second [x] (k/first (k/next x)))
(defn ffirst [x] (first (first x)))
(defn nfirst [x] (next (first x)))
(defn fnext [x] (first (next x)))
(defn nnext [x] (next (next x)))
(defn count [x] (k/count x))
(defn last [x] (loop [s (seq x)] (if (next s) (recur (next s)) (first s))))
(defn empty? [x] (k/nil? (seq x)))
(defn not-empty [x] (when (seq x) x))
(defn list [& xs] (k/apply k/list xs))
(defn cons [x s] (k/cons x s))
(defn concat [& xs] (k/apply k/concat xs))
(defn vec [x] (k/vec x))
(defn vector [& xs] (k/vec xs))

(defn- reverse-list [acc]
  (loop [s (seq acc) r ()] (if s (recur (next s) (k/cons (first s) r)) r)))
(defn reverse [x] (reverse-list x))
(defn butlast [x]
  (loop [s (seq x) acc []]
    (if (next s) (recur (next s) (k/conj acc (first s))) (seq acc))))
(defn list* [& args]
  (k/concat (butlast args) (last args)))

(defn- apply-args [args]
  (if (next args)
    (k/cons (first args) (apply-args (next args)))
    (seq (first args))))
(defn apply [f & args]
  (k/apply f (apply-args args)))

(defn map
  ([f c] (loop [s (seq c) acc []] (if s (recur (next s) (k/conj acc (f (first s)))) (seq acc))))
  ([f c1 c2] (loop [a (seq c1) b (seq c2) acc []]
               (if (and a b) (recur (next a) (next b) (k/conj acc (f (first a) (first b)))) (seq acc))))
  ([f c1 c2 c3] (loop [a (seq c1) b (seq c2) c (seq c3) acc []]
                  (if (and a b c)
                    (recur (next a) (next b) (next c) (k/conj acc (f (first a) (first b) (first c))))
                    (seq acc)))))
(defn mapcat [f & cs] (apply concat (apply map f cs)))
(defn filter [pred c] (loop [s (seq c) acc []]
                        (if s (recur (next s) (if (pred (first s)) (k/conj acc (first s)) acc)) (seq acc))))
(defn remove [pred c] (filter (fn [x] (not (pred x))) c))
(defn keep [f c] (filter some? (map f c)))
(defn every? [pred c] (loop [s (seq c)] (if s (if (pred (first s)) (recur (next s)) false) true)))
(defn some [pred c] (loop [s (seq c)] (when s (or (pred (first s)) (recur (next s))))))
(defn not-every? [pred c] (not (every? pred c)))
(defn not-any? [pred c] (not (some pred c)))
(defn reduce
  ([f c] (let [s (seq c)] (if s (reduce f (first s) (next s)) (f))))
  ([f init c] (loop [acc init s (seq c)] (if s (recur (f acc (first s)) (next s)) acc))))
(defn take [n c] (loop [n n s (seq c) acc []]
                   (if (and s (k/> n 0)) (recur (k/- n 1) (next s) (k/conj acc (first s))) (seq acc))))
(defn drop [n c] (loop [n n s (seq c)] (if (and s (k/> n 0)) (recur (k/- n 1) (next s)) s)))
(defn take-while [pred c] (loop [s (seq c) acc []]
                            (if (and s (pred (first s))) (recur (next s) (k/conj acc (first s))) (seq acc))))
(defn drop-while [pred c] (loop [s (seq c)] (if (and s (pred (first s))) (recur (next s)) s)))
(defn take-nth [n c] (loop [s (seq c) acc []] (if s (recur (drop n s) (k/conj acc (first s))) (seq acc))))
(defn split-at [n c] (vector (take n c) (drop n c)))
(defn split-with [pred c] (vector (take-while pred c) (drop-while pred c)))
(defn nth
  ([c n] (k/nth c n))
  ([c n not-found] (k/nth c n not-found)))
(defn partition
  ([n c] (partition n n c))
  ([n step c] (loop [s (seq c) acc []]
                (let [p (take n s)]
                  (if (k/= (count p) n)
                    (recur (drop step s) (k/conj acc p))
                    (seq acc))))))
(defn partition-all [n c] (loop [s (seq c) acc []]
                            (if s (recur (drop n s) (k/conj acc (take n s))) (seq acc))))
(defn interleave [a b] (mapcat list a b))
(defn interpose [sep c] (rest (mapcat (fn [x] (list sep x)) c)))
(defn range
  ([n] (range 0 n))
  ([a b] (loop [i a acc []] (if (k/< i b) (recur (k/+ i 1) (k/conj acc i)) (seq acc)))))
(defn repeat [n x] (loop [i 0 acc []] (if (k/< i n) (recur (k/+ i 1) (k/conj acc x)) (seq acc))))
(defn identity [x] x)
(defn constantly [x] (fn [& args] x))
(defn comp [& fs] (fn [x] (reduce (fn [acc f] (f acc)) x (reverse fs))))
(defn partial [f & args] (fn [& more] (apply f (concat args more))))

;;; Numbers

(defn + ([] 0) ([x] x) ([x y] (k/+ x y)) ([x y & m] (k/apply k/+ x y m)))
(defn - ([x] (k/- x)) ([x y] (k/- x y)) ([x y & m] (k/apply k/- x y m)))
(defn * ([] 1) ([x] x) ([x y] (k/* x y)) ([x y & m] (k/apply k/* x y m)))
(defn / ([x] (k// x)) ([x y] (k// x y)))
(defn inc [x] (k/+ x 1))
(defn dec [x] (k/- x 1))
(defn < [& xs] (k/apply k/< xs))
(defn > [& xs] (k/apply k/> xs))
(defn <= [& xs] (k/apply k/<= xs))
(defn >= [& xs] (k/apply k/>= xs))
(defn == [& xs] (k/apply k/= xs))
(defn zero? [x] (k/= x 0))
(defn pos? [x] (k/> x 0))
(defn neg? [x] (k/< x 0))
(defn even? [x] (k/= (k/mod x 2) 0))
(defn odd? [x] (k/= (k/mod x 2) 1))
(defn max [& xs] (k/apply k/max xs))
(defn min [& xs] (k/apply k/min xs))
(defn mod [a b] (k/mod a b))
(defn rem [a b] (k/rem a b))
(defn quot [a b] (k/truncate a b))

;;; Equality

(defn =
  ([x] true)
  ([a b] (k/equiv a b))
  ([a b & more] (if (k/equiv a b) (apply = b more) false)))
(defn not= [& args] (not (apply = args)))

;;; Maps and sets

(defn get
  ([m k] (k/get m k))
  ([m k not-found] (k/get m k not-found)))
(defn contains? [m k] (k/contains? m k))
(defn key [e] (k/nth e 0))
(defn val [e] (k/nth e 1))
(defn keys [m] (map key m))
(defn vals [m] (map val m))
(defn find [m k] (when (contains? m k) (vector k (get m k))))
(defn assoc
  ([m k v] (k/assoc m k v))
  ([m k v & kvs] (apply assoc (k/assoc m k v) kvs)))
(defn dissoc [m & ks]
  (reduce (fn [acc e] (if (some (fn [k] (= k (key e))) ks) acc (k/assoc acc (key e) (val e))))
          (k/with-meta {} (k/meta m)) m))
(defn conj
  ([c x] (k/conj c x))
  ([c x & xs] (reduce k/conj (k/conj c x) xs)))
(defn disj [s & xs]
  (reduce (fn [acc x] (if (some (fn [y] (= x y)) xs) acc (k/conj acc x))) #{} s))
(defn into [to from] (reduce conj to from))
(defn merge [& ms] (reduce (fn [a m] (reduce conj (or a {}) m)) nil (filter some? ms)))
(defn zipmap [ks vs]
  (loop [m {} k (seq ks) v (seq vs)]
    (if (and k v) (recur (k/assoc m (first k) (first v)) (next k) (next v)) m)))
(defn hash-map [& kvs] (k/apply k/array-map kvs))
(defn array-map [& kvs] (k/apply k/array-map kvs))
(defn hash-set [& xs] (k/apply k/hash-set xs))
(defn set [x] (k/apply k/hash-set (seq x)))
(defn distinct [c] (seq (set c)))
(defn update [m k f & args] (assoc m k (apply f (get m k) args)))
(defn select-keys [m ks] (reduce (fn [r k] (if (contains? m k) (assoc r k (get m k)) r)) {} ks))
(defn empty [c] (cond (vector? c) [] (map? c) {} (set? c) #{} (seq? c) () :else nil))

;;; Strings, symbols, keywords

(defn str
  ([] "")
  ([x] (k/str x))
  ([x & ys] (k/apply k/string-concat (k/str x) (map k/str ys))))
(defn name [x] (if (k/string? x) x (k/name x)))
(defn namespace [x] (k/namespace x))
(defn symbol
  ([n] (if (symbol? n) n (k/symbol nil (name n))))
  ([ns n] (k/symbol ns n)))
(defn keyword
  ([n] (if (keyword? n) n (k/keyword nil (name n))))
  ([ns n] (k/keyword ns n)))
(defn gensym ([] (k/gensym "G__")) ([prefix] (k/gensym prefix)))
(defn subs ([s start] (k/subseq s start)) ([s start end] (k/subseq s start end)))
(defn pr-str [& xs] (apply str (interpose " " (map k/pr-str xs))))
(defn meta [x] (k/meta x))
(defn with-meta [x m] (k/with-meta x m))
(defn vary-meta [x f & args] (with-meta x (apply f (meta x) args)))


;;; Namespaces

(defn- argument-error [msg]
  (k/make-instance (k/kernel-type "corten.lang/ArgumentError") msg nil))
(defn the-ns [x] (if (symbol? x) (or (k/find-ns x) (throw (argument-error (str "No namespace: " x)))) x))
(defn find-ns [x] (k/find-ns x))
(defn create-ns [x] (k/create-ns x))
(defn resolve [sym] (k/resolve sym))
(defn macroexpand-1 [form] (k/macroexpand-1 form))
(defn macroexpand [form]
  (loop [f form]
    (let [e (macroexpand-1 f)] (if (identical? e f) f (recur e)))))
(defn eval [form] (k/eval form))
