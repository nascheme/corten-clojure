;;;; require, use, refer, alias, import and load as functions, and the
;;;; ns macro calling them (after clojure.core and SCI's impl/load.cljc).
;;;; The kernel finds and loads lib files (k/require: the load path,
;;;; .cljs before .cljc before .clj, clojure.x falling back to cljs.x,
;;;; cyclic loads) and keeps the set of loaded libs.

(in-ns 'clojure.core)

(defn- throw-arg [& msg]
  (throw (corten.lang/ArgumentError. (apply str msg))))

(defn loaded-libs
  "Returns a sorted set of symbols naming the currently loaded libs"
  []
  (into (sorted-set) (map symbol) (k/loaded-libs)))

(defn- load-lib
  "The namespace of lib, loaded from its file unless it is loaded (or
  reload is true). If loading fails, a namespace it created is removed."
  [lib reload macros?]
  (let [existed (find-ns lib)]
    (try
      (if macros?
        (k/require-macros (name lib) reload)
        (k/require (name lib) reload))
      (catch :default e
        (when (and (not existed) (find-ns lib) (not= (find-ns lib) *ns*))
          (remove-ns lib))
        (throw e)))))

(defn- refer-names
  "Refer the vars named in names (symbols) of namespace lns in the current
  namespace, under their names in rename if there."
  [lns names rename]
  (doseq [n names]
    (if-let [local (get rename n)]
      (k/refer-name *ns* lns (name n) (name local))
      (k/refer-name *ns* lns (name n)))))

(defn- refer-ns
  "refer for namespace lns: :only (or :refer) names, :exclude, :rename."
  [lns {:keys [only exclude rename]}]
  (if only
    (refer-names lns only rename)
    (do (k/refer *ns* lns (map name (concat exclude (keys rename))))
        (refer-names lns (keys rename) rename))))

(defn refer
  "refers to all public vars of ns, subject to filters.
  filters can include at most one each of:

  :exclude list-of-symbols
  :only list-of-symbols
  :rename map-of-fromsymbol-tosymbol"
  [ns-sym & filters]
  (let [lns (or (find-ns ns-sym) (throw-arg "No namespace: " ns-sym))]
    (refer-ns lns (apply hash-map filters))))

(defn- apply-lib-opts
  "Alias and refer lib's namespace lns as the libspec options say."
  [lib lns {:keys [as as-alias refer refer-macros exclude rename]}]
  (when as (k/alias *ns* (name as) lns))
  (when as-alias (k/alias *ns* (name as-alias) lns))
  (cond
    (= refer :all) (refer-ns lns {:exclude exclude :rename rename})
    refer (refer-names lns refer rename))
  (refer-names lns refer-macros rename))

(defn- libspecs
  "The [lib opts] of require or use arguments: symbols, vectors [lib
  opt*] and prefix lists (prefix libspec*)."
  [args]
  (let [spec (fn [prefix s]
               (let [[lib opts] (if (vector? s) [(first s) (apply hash-map (rest s))] [s {}])]
                 (when-not (symbol? lib)
                   (throw-arg "Unsupported libspec: " (pr-str s)))
                 [(if prefix (symbol (str prefix "." lib)) lib) opts]))]
    (mapcat (fn [a]
              (cond
                (keyword? a) nil
                (or (symbol? a) (vector? a)) [(spec nil a)]
                (seq? a) (map #(spec (first a) %) (rest a))
                :else (throw-arg "Unsupported libspec: " (pr-str a))))
            args)))

(defn- use-opts
  "use's options as require's: refer all, or the :only names."
  [opts]
  (assoc (dissoc opts :only) :refer (if (contains? opts :only) (:only opts) :all)))

(defn- load-libs [args macros? use?]
  (let [flags (set (filter keyword? args))
        reload (boolean (or (flags :reload) (flags :reload-all)))]
    (doseq [[lib opts] (libspecs args)]
      (let [opts (if use? (use-opts opts) opts)]
        (if (and (:as-alias opts) (not (:as opts)) (not (:refer opts))
                 (not (:refer-macros opts)))
          ;; :as-alias only: the namespace is not loaded.
          (k/alias *ns* (name (:as-alias opts)) (create-ns lib))
          (apply-lib-opts lib (load-lib lib reload macros?) opts))))
    nil))

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that
  modifies how all the identified libs are loaded (:reload,
  :reload-all). A libspec is a lib name or a vector [lib :as alias
  :as-alias alias :refer [names] or :all :rename {name local}]."
  [& args]
  (load-libs args false false))

(defn use
  "Like require, but also refers to each lib's namespace using
  clojure.core/refer (options :only, :exclude, :rename)."
  [& args]
  (load-libs args false true))

(defn ^:private require-macros*
  "(:require-macros libspec*) of ClojureScript: as require, except that
  a namespace requiring its own macros loads its .clj or .cljc file."
  [& args]
  (load-libs args true false))

(defn ^:private use-macros*
  [& args]
  (load-libs args true true))

(defn alias
  "Add an alias in the current namespace to another namespace."
  [alias namespace-sym]
  (k/alias *ns* (name alias) (the-ns namespace-sym))
  nil)

(defn- root-resource
  "The resource path of lib: a.b-c is /a/b_c."
  [lib]
  (apply str "/" (map #(case % \- \_ \. \/ %) (name lib))))

(defn load
  "Loads Clojure code from resources on the load path. A path is
  interpreted as load-path-relative if it begins with a slash or
  relative to the root directory of the current namespace otherwise."
  [& paths]
  (doseq [p paths]
    (let [p (if (= "/" (subs p 0 1))
              p
              (let [r (root-resource (ns-name *ns*))]
                (str (subs r 0 (inc (or (corten.string/last-index-of r "/") -1))) p)))]
      (k/load-resource (subs p 1))))
  nil)

(defn- import-types
  "Refer the types named by specs: [lib Type*] or lib.Type."
  [specs]
  (doseq [s specs]
    (let [[lib types] (if (sequential? s)
                        [(first s) (rest s)]
                        (let [n (name s) i (corten.string/last-index-of n ".")]
                          [(symbol (subs n 0 i)) [(symbol (subs n (inc i)))]]))
          lns (load-lib lib false false)]
      (refer-names lns types nil))))

(defmacro import
  "import-list => (package-symbol class-name-symbols*): refer the types
  of a namespace (the package)."
  [& import-symbols-or-lists]
  `(import-types '~import-symbols-or-lists))

(defmacro require-macros [& specs]
  `(require-macros* ~@specs))

(defmacro refer-clojure
  "Same as (refer 'clojure.core <filters>)"
  [& filters]
  `(refer '~'clojure.core ~@(map #(list 'quote %) filters)))

(defmacro ns
  "Sets *ns* to the namespace named by name (unevaluated), creating it
  if needed. references can be zero or more of: (:refer-clojure ...)
  (:require ...) (:use ...) (:import ...) (:load ...) (:require-macros
  ...) (:use-macros ...), with the syntax of the functions."
  [name & references]
  (let [doc (when (string? (first references)) (first references))
        references (if doc (rest references) references)
        attrs (when (map? (first references)) (first references))
        references (if attrs (rest references) references)
        m (merge (meta name) attrs (when doc {:doc doc}))
        refs (filter seq? references)
        rc (some #(when (= :refer-clojure (first %)) %) refs)
        quoted (fn [args] (map #(list 'quote %) args))
        clause (fn [[k & args]]
                 (case k
                   :require `(require ~@(quoted args))
                   :use `(use ~@(quoted args))
                   :require-macros `(require-macros* ~@(quoted args))
                   :use-macros `(use-macros* ~@(quoted args))
                   :import `(import-types '~args)
                   :load `(load ~@args)
                   :refer-clojure nil
                   :gen-class nil
                   (throw-arg "Unsupported ns clause: " k)))]
    `(do (k/in-ns '~name nil)
         ~@(when (seq m) [`(reset-meta! *ns* '~m)])
         ~@(when-not (= name 'clojure.core)
             [`(refer '~'clojure.core ~@(quoted (rest rc)))])
         ~@(keep clause refs)
         (k/add-loaded-lib ~(clojure.core/name name))
         nil)))

;; The libs loaded so far (by the boot files, without the ns function).
(doseq [ns (all-ns)]
  (k/add-loaded-lib (k/ns-name ns)))
