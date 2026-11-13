;; PORT: the compiler state of cljs.env, for macros that read compiler
;; options (cljs.spec: :elide-asserts). There are no options.
(ns cljs.env)

(def ^:dynamic *compiler* (atom {:options {}}))
