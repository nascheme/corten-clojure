;;;; Builds the Clojure image from the kernel alone:
;;;;   corten --boot clj/init.clj --save clj.img
;;;; The Clojure bootstrap, cljs.core, the functions defined after it and
;;;; the REPL.
;;;; Paths are relative to this directory.

(k/load "bootstrap.clj")
(k/load "boot.clj")
(k/load "macros.clj")
(k/add-load-path ".")
(k/load "cljs/core.cljs")
(k/load "core-extras.clj")
(k/load "load.clj")
(k/load "repl.clj")
;; Clojure code starts in the user namespace.
(k/in-ns 'user)
