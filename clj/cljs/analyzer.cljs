;; PORT: the parts of the ClojureScript analyzer that macros use
;; (cljs.spec). There is no analyzer environment: &env is nil, and names
;; resolve in the current namespace, ignoring locals.
(ns cljs.analyzer)

(def ^:dynamic *private-var-access-nowarn* false)

(defn var-map
  "The analyzer map of var v: :name (a qualified symbol), :ns, and the
  :macro and :const flags."
  [v]
  (let [m (meta v)]
    (cond-> {:name (symbol (str (:ns m)) (str (:name m))) :ns (:ns m)}
      (:macro m) (assoc :macro true)
      (:const m) (assoc :const true))))

(defn resolve-var
  "The analyzer map of the var symbol sym names in the current namespace.
  A symbol naming no var resolves to itself qualified by the current
  namespace (or by its own namespace)."
  [env sym]
  (if-let [v (resolve sym)]
    (var-map v)
    {:name (if (namespace sym) sym (symbol (str (ns-name *ns*)) (name sym)))
     :ns (if (namespace sym) (symbol (namespace sym)) (ns-name *ns*))}))
