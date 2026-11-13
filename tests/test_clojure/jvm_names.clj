;;;; The JVM exception classes that Clojure's tests name, as the
;;;; corten.lang error types (loaded by scripts/clojure-tests.sh only).
;;;; Unqualified names fall back to corten.lang vars in every namespace;
;;;; java.lang.X and clojure.lang.X resolve to java.lang/X and
;;;; clojure.lang/X.

(ns corten.test.jvm-names)

(def names
  '{Throwable Error, Exception Error, RuntimeException Error
    IllegalArgumentException ArgumentError
    NumberFormatException ArgumentError
    ClassCastException TypeError, NullPointerException TypeError
    ArithmeticException ArithmeticError
    IndexOutOfBoundsException IndexError
    ArrayIndexOutOfBoundsException IndexError
    StringIndexOutOfBoundsException IndexError
    IllegalStateException StateError
    UnsupportedOperationException UnsupportedError})

(doseq [[jvm ours] names
        ns '[corten.lang java.lang]]
  (intern (create-ns ns) jvm @(resolve ours)))

(doseq [[jvm ours] '{ExceptionInfo ExceptionInfo, ArityException ArityError}]
  (intern (create-ns 'clojure.lang) jvm @(resolve ours)))
