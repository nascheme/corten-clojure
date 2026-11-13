;;;; clojure.core functions defined once cljs.core is loaded: reading,
;;;; vars and namespaces, format, regex matchers.

(in-ns 'clojure.core)

;; bounded: printing for an error message, so an infinite lazy seq or a
;; huge collection is cut short.
(defn- print-hook [x readably bounded]
  (if bounded
    (binding [*print-readably* readably
              *print-length* (or *print-length* 20)
              *print-level* (or *print-level* 5)]
      (pr-str x))
    (binding [*print-readably* readably]
      (pr-str x))))

(k/set-hook "print" print-hook)

(def ^{:doc "The bindings of let* for destructuring bindings (the
  function the let, fn and loop macros use)."}
  destructure clojure.core$macros/destructure)

(defn read-string
  "Reads one object from the string s."
  ([s] (k/read-string s))
  ([opts s]
   (if (contains? opts :eof)
     (k/read (k/string-input-stream s) false (:eof opts) :data)
     (k/read-string s))))

(def ^{:dynamic true
       :doc "A PushbackReader for standard input: read and read-line read from *in*."}
  *in* (k/make-instance corten.lang/PushbackReader nil))

(defn read
  "Reads the next object from stream, a PushbackReader (default *in*).
  With eof-error? false, returns eof-value at the end; (read opts stream)
  takes :eof."
  ([] (read *in*))
  ([stream] (read stream true nil))
  ([stream eof-error? eof-value]
   (k/read (.-stream stream) eof-error? eof-value :data))
  ([stream eof-error? eof-value _recursive?]
   (read stream eof-error? eof-value))
  ([opts stream]
   (if (contains? opts :eof)
     (read stream false (:eof opts))
     (read stream))))

(defn read-line
  "Reads the next line from *in*, or nil at the end."
  []
  (.readLine *in*))

(defn line-seq
  "The lines of rdr (a reader) as a lazy seq."
  [rdr]
  (when-let [line (.readLine rdr)]
    (cons line (lazy-seq (line-seq rdr)))))

(set! *eval* (fn [form] (k/eval form)))

(defn macroexpand-1
  "If form is a macro call, returns its expansion, else form."
  [form]
  (try
    (k/macroexpand-1 form)
    (catch corten.lang/ArityError e
      ;; The macro's function also takes &form and &env (as Clojure's
      ;; Compiler.macroexpand1 reports it).
      (if (and (seq? form) (symbol? (first form))
               (corten.string/ends-with? (str "/" (.-name e)) (str "/" (name (first form)))))
        (throw (corten.lang/ArityError. (- (.-actual e) 2) (.-name e)))
        (throw e)))))

(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer represents
  a macro form, then returns it."
  [form]
  (let [ex (macroexpand-1 form)]
    (if (identical? ex form) form (if (= ex form) form (macroexpand ex)))))

;; with-meta on a var: a view of it with other metadata (cljs Var).
(deftype VarWithMeta [v meta]
  IDeref
  (-deref [_] @v)
  IMeta
  (-meta [_] meta)
  IWithMeta
  (-with-meta [_ m] (VarWithMeta. v m))
  IEquiv
  (-equiv [_ o] (or (identical? v o) (and (instance? VarWithMeta o) (identical? v (.-v o)))))
  IHash
  (-hash [_] (hash v))
  IPrintWithWriter
  (-pr-writer [_ w opts] (let [m (clojure.core/meta v)] (-write w (str "#'" (:ns m) "/" (:name m)))))
  Fn
  IFn
  (-invoke [_] (@v))
  (-invoke [_ a] (@v a))
  (-invoke [_ a b] (@v a b))
  (-invoke [_ a b c] (@v a b c))
  (-invoke [_ a b c d] (@v a b c d))
  (-invoke [_ a b c d e] (@v a b c d e))
  (-invoke [_ a b c d e f] (@v a b c d e f))
  (-invoke [_ a b c d e f g] (@v a b c d e f g))
  (-invoke [_ a b c d e f g h] (@v a b c d e f g h))
  (-invoke [_ a b c d e f g h i] (@v a b c d e f g h i))
  (-invoke [_ a b c d e f g h i j] (@v a b c d e f g h i j))
  (-invoke [_ a b c d e f g h i j k] (@v a b c d e f g h i j k))
  (-invoke [_ a b c d e f g h i j k l] (@v a b c d e f g h i j k l))
  (-invoke [_ a b c d e f g h i j k l m] (@v a b c d e f g h i j k l m))
  (-invoke [_ a b c d e f g h i j k l m n] (@v a b c d e f g h i j k l m n))
  (-invoke [_ a b c d e f g h i j k l m n o] (@v a b c d e f g h i j k l m n o))
  (-invoke [_ a b c d e f g h i j k l m n o p] (@v a b c d e f g h i j k l m n o p))
  (-invoke [_ a b c d e f g h i j k l m n o p q] (@v a b c d e f g h i j k l m n o p q))
  (-invoke [_ a b c d e f g h i j k l m n o p q r] (@v a b c d e f g h i j k l m n o p q r))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s] (@v a b c d e f g h i j k l m n o p q r s))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s t] (@v a b c d e f g h i j k l m n o p q r s t))
  (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest] (apply @v a b c d e f g h i j k l m n o p q r s t rest)))

;; Vars (kernel objects): @#'v is the current value. The metadata given
;; to def (calls in it evaluated, the rest as read) gets :ns and :name
;; when read (k/var-meta); alter-meta!, reset-meta! and intern store a map.
(defn- var-ns-sym [v]
  (let [ns (k/ns-name (k/var-ns v))]
    (symbol (if (and (>= (count ns) 7)
                     (k/= (k/string-index-of ns "$macros") (- (count ns) 7)))
              (subs ns 0 (- (count ns) 7))
              ns))))

(extend-type Var
  IDeref
  (-deref [v] (k/var-get v))
  IMeta
  (-meta [v] (k/var-meta v))
  IWithMeta
  (-with-meta [v m] (VarWithMeta. v m))
  IWatchable
  (-notify-watches [v old new]
    (doseq [[k f] (.-watches v)] (f k v old new)))
  (-add-watch [v k f]
    (set! (.-watches v) (assoc (.-watches v) k f))
    v)
  (-remove-watch [v k]
    (set! (.-watches v) (dissoc (.-watches v) k))
    v))

(k/set-hook "var-watch" (fn [v old new] (-notify-watches v old new)))

(extend-type Namespace
  IMeta
  (-meta [ns] (.-meta ns))
  IPrintWithWriter
  (-pr-writer [ns w _] (-write w (str "#namespace[" (k/ns-name ns) "]"))))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  [iref f & args]
  (if (var? iref)
    (k/var-set-meta iref (apply f (meta iref) args))
    (set! (.-meta iref) (apply f (.-meta iref) args))))

(defn reset-meta!
  "Atomically resets the metadata for a namespace/var/ref/agent/atom"
  [iref m]
  (if (var? iref)
    (k/var-set-meta iref m)
    (set! (.-meta iref) m)))

;;; Namespace introspection (macros in cljs; functions here).

(defn- the-ns* [ns]
  (if (symbol? ns)
    (or (k/find-ns (name ns)) (throw (corten.lang/ArgumentError. (str "No namespace: " ns))))
    ns))

(defn- ns-entries [ns pred]
  (let [ns (the-ns* ns)]
    (loop [es (k/ns-entries ns) m {}]
      (if es
        (let [[n v] (first es)]
          (recur (next es)
                 (if (pred ns v) (assoc m (symbol n) v) m)))
        m))))

(defn- home-ns [v] (k/var-ns v))

(defn ns-name
  "The name of the namespace (or namespace name) ns, a symbol."
  [ns]
  (symbol (k/ns-name (the-ns* ns))))

(defn all-ns
  "A sequence of all namespaces."
  []
  (k/all-ns))

(defn ns-interns
  "A map of the intern mappings for the namespace."
  [ns]
  (ns-entries ns (fn [ns v] (identical? (home-ns v) ns))))

(defn ns-publics
  "A map of the public intern mappings for the namespace."
  [ns]
  (ns-entries ns (fn [ns v] (and (identical? (home-ns v) ns) (not (:private (meta v)))))))

(defn ns-imports
  "A map of the types referred by the namespace (not from clojure.core)."
  [ns]
  (let [core (k/find-ns "clojure.core")]
    (into {} (map (fn [[k v]] [k @v]))
          (ns-entries ns (fn [ns v] (and (not (identical? (home-ns v) ns))
                                         (not (identical? (home-ns v) core))
                                         (k/var-bound? v)
                                         (k/type? @v)))))))

(defn resolve
  "The var to which symbol sym resolves in the current namespace, or nil."
  [sym]
  (let [v (k/resolve sym)]
    (when (and v (k/var? v)) v)))

(defn ns-resolve
  "Returns the var or Class to which a symbol will be resolved in the
  namespace (unless found in the environment), else nil."
  ([ns sym] (ns-resolve ns nil sym))
  ([ns env sym]
   (when-not (contains? env sym)
     (binding [*ns* (the-ns* ns)] (resolve sym)))))

(defn the-ns
  "If passed a namespace, returns it. Else, when passed a symbol,
  returns the namespace named by it, throwing an exception if not
  found."
  [x]
  (the-ns* x))

(defn ns-map
  "Returns a map of all the mappings for the namespace."
  [ns]
  (ns-entries ns (fn [_ _] true)))

(defn ns-refers
  "Returns a map of the refer mappings for the namespace."
  [ns]
  (ns-entries ns (fn [ns v] (not (identical? (home-ns v) ns)))))

(defn ns-aliases
  "Returns a map of the aliases for the namespace."
  [ns]
  (into {} (map (fn [[a ns]] [(symbol a) ns])) (k/ns-aliases (the-ns* ns))))

(defn ns-unmap
  "Removes the mappings for the symbol from the namespace."
  [ns sym]
  (k/ns-unmap (the-ns* ns) (name sym))
  nil)

(defn ns-unalias
  "Removes the alias for the symbol from the namespace."
  [ns sym]
  (k/ns-unalias (the-ns* ns) (name sym))
  nil)

(defn remove-ns
  "Removes the namespace named by the symbol. Use with caution.
  Cannot be used to remove the clojure namespace."
  [sym]
  (when (= (str sym) "clojure.core")
    (throw (corten.lang/ArgumentError. "Cannot remove clojure namespace")))
  (when-let [ns (find-ns sym)]
    (k/remove-ns ns)))

(defn intern
  "Finds or creates a var named by the symbol name in the namespace
  ns (which can be a symbol or a namespace), setting its root binding
  to val if supplied. The namespace must exist. The var will adopt any
  metadata from the name symbol. Returns the var."
  ([ns name]
   (let [ns (the-ns* ns)
         v (k/intern ns (clojure.core/name name))]
     (when-let [m (meta name)]
       (k/var-set-meta v (assoc m :ns (var-ns-sym v) :name (symbol (clojure.core/name name)))))
     v))
  ([ns name val]
   (let [v (intern ns name)]
     (k/var-set-root v val)
     v)))

(defn find-var
  "Returns the global var named by the namespace-qualified symbol, or
  nil if no var with that name."
  [sym]
  (when-not (namespace sym)
    (throw (corten.lang/ArgumentError. "Symbol must be namespace-qualified")))
  (let [ns (or (find-ns (symbol (namespace sym)))
               (throw (corten.lang/ArgumentError. (str "No such namespace: " (namespace sym)))))
        v (get (ns-interns ns) (symbol (name sym)))]
    v))

(defn var-get
  "Gets the value in the var object"
  [x]
  (k/var-get x))

(defn var-set
  "Sets the value in the var object to val. The var must be
  thread-locally bound."
  [x val]
  (when-not (k/var-thread-bound? x)
    (throw (corten.lang/StateError.
            (str "Can't change/establish root binding of: " (symbol x) " with set"))))
  (set! (.-val x) val))

(defn alter-var-root
  "Atomically alters the root binding of var v by applying f to its
  current value plus any args"
  [v f & args]
  (k/var-set-root v (apply f (k/var-root v) args)))

(defn bound?
  "Returns true if all of the vars provided as arguments have any bound
  value, root or thread-local."
  [& vars]
  (every? #(k/var-bound? %) vars))

(defn thread-bound?
  "Returns true if all of the vars provided as arguments have
  thread-local bindings."
  [& vars]
  (every? #(k/var-thread-bound? %) vars))

(defn get-thread-bindings
  "Get a map with the Var/value pairs which is currently in effect for
  the current thread."
  []
  (into {} (k/thread-bindings)))

(defn with-bindings*
  "Takes a map of Var/value pairs. Installs for the given Vars the
  associated values as thread-local bindings. Then calls f with the
  supplied arguments. Pops the installed bindings after f returned.
  Returns whatever f returns."
  [binding-map f & args]
  (let [ks (keys binding-map)]
    (k/progv ks (map #(get binding-map %) ks)
               (fn [] (apply f args)))))

(defn bound-fn*
  "Returns a function, which will install the same bindings in effect
  as in the thread at the time bound-fn* was called and then call f
  with any given arguments."
  [f]
  (let [bindings (get-thread-bindings)]
    (fn [& args]
      (apply with-bindings* bindings f args))))

(defn with-redefs-fn
  "Temporarily redefines Vars during a call to func. Each val of
  binding-map will replace the root value of its key which must be a
  Var. After func is called with no args, the root values of all the
  Vars will be set back to their old values."
  [binding-map func]
  (let [root-bind (fn [m] (doseq [[v x] m] (k/var-set-root v x)))
        old-vals (zipmap (keys binding-map) (map #(k/var-root %) (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

(def ^{:dynamic true
       :doc "The Clojure version this implements, as a map of :major
  :minor :incremental and :qualifier."}
  *clojure-version* {:major 1 :minor 12 :incremental 0 :qualifier nil})

(defn clojure-version
  "Returns clojure version as a printable string."
  []
  (str (:major *clojure-version*)
       "."
       (:minor *clojure-version*)
       (when-let [i (:incremental *clojure-version*)]
         (str "." i))
       (when-let [q (:qualifier *clojure-version*)]
         (when (pos? (count q)) (str "-" q)))
       (when (:interim *clojure-version*)
         "-SNAPSHOT")))

;; Accepted and ignored: there is no reflection, and arithmetic is
;; always checked.
(def ^:dynamic *warn-on-reflection* false)
(def ^:dynamic *unchecked-math* false)
;; The reader has no #= (read-eval) forms; clojure.repl/source checks
;; for :unknown as Clojure does.
(def ^:dynamic *read-eval* true)

(defn format
  "Formats a string using java.util.Formatter's syntax (src/clj/format.rs:
  no %t dates, %h or %a)."
  [fmt & args]
  (k/format fmt (to-array args) str))

(defn printf
  "Prints formatted output, as per format"
  [fmt & args]
  (print (apply format fmt args)))

;; A StringBuilder is a CharSequence: count, nth and seq as on a string.
(extend-type StringBuilder
  ICounted
  (-count [sb] (.length sb))
  IIndexed
  (-nth
    ([sb n] (nth (.toString sb) n))
    ([sb n not-found] (nth (.toString sb) n not-found)))
  ISeqable
  (-seq [sb] (seq (.toString sb))))

;;; Regex matchers (java.util.regex.Matcher): re-matcher, re-groups and
;;; re-find of a matcher.

(defn- match-groups [m]
  (let [a (k/nth m 1)]
    (if (== (alength a) 1) (aget a 0) (vec a))))

(deftype Matcher [re s ^:mutable pos ^:mutable m]
  Object
  (find [_]
    (if (<= pos (count s))
      (let [r (k/regex-exec (.-source re) (.-flags re) s pos)]
        (set! m r)
        (if (nil? r)
          (do (set! pos (inc (count s))) false)
          (let [start (k/nth r 0)
                end (+ start (count (aget (k/nth r 1) 0)))]
            ;; After an empty match the next search starts one further.
            (set! pos (if (== start end) (inc end) end))
            true)))
      (do (set! m nil) false)))
  (matches [this]
    (set! pos 0)
    (and (.find this) (== 0 (.start this)) (== (count s) (.end this))))
  (reset [this] (set! pos 0) (set! m nil) this)
  (groupCount [_] (k/regex-group-count (.-source re) (.-flags re)))
  (group [this] (.group this 0))
  (group [_ i]
    (when (nil? m) (throw (StateError. "No match found")))
    (let [a (k/nth m 1)]
      (if (and (<= 0 i) (< i (alength a)))
        (aget a i)
        (throw (IndexError. (str "No group " i))))))
  (start [_]
    (when (nil? m) (throw (StateError. "No match available")))
    (k/nth m 0))
  (end [this] (+ (.start this) (count (.group this 0))))
  IIndexed
  (-nth [this i] (.group this i))
  (-nth [this i not-found]
    (if (and m (<= 0 i) (< i (alength (k/nth m 1))))
      (aget (k/nth m 1) i)
      not-found)))

(defn re-matcher
  "Returns an instance of Matcher, for use, e.g. in re-find."
  [re s]
  (if (string? s)
    (Matcher. re s 0 nil)
    (throw (TypeError. "re-matcher must match against a string."))))

(defn re-groups
  "Returns the groups from the most recent match/find. If there are no
  nested groups, returns a string of the entire match. If there are
  nested groups, returns a vector of the groups, the first element
  being the entire match."
  [m]
  (when (nil? (.-m m)) (throw (StateError. "No match found")))
  (match-groups (.-m m)))

(let [re-find-2 re-find]
  (defn re-find
    "Returns the next regex match, if any, of string to pattern, using
  Matcher.find().  Uses re-groups to return the groups."
    ([m] (when (.find m) (re-groups m)))
    ([re s] (re-find-2 re s))))
