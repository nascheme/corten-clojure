;; PORT: cljs.analyzer.api on the stand-in cljs.analyzer.
(ns cljs.analyzer.api
  (:refer-clojure :exclude [resolve ns-interns])
  (:require [cljs.analyzer :as ana]))

(defn resolve
  "The analyzer map of the var symbol sym names in the current
  namespace (:name, :ns, :macro), or nil."
  [env sym]
  (when (clojure.core/resolve sym)
    (ana/resolve-var env sym)))

(defn ns-interns
  "A map of the names of the vars interned in namespace ns (a symbol) to
  their analyzer maps."
  [ns]
  (into {} (map (fn [[k v]] [k (ana/var-map v)])) (clojure.core/ns-interns ns)))
