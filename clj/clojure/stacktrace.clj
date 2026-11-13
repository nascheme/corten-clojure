;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;;; PORT: from Clojure src/clj/clojure/stacktrace.clj. Errors have no
;;;; stack traces, so only the error and its causes are printed.

(ns ^{:doc "Print stack traces oriented towards Clojure, not Java."
       :author "Stuart Sierra"}
  clojure.stacktrace)

(defn root-cause
  "Returns the last 'cause' Throwable in a chain of Throwables."
  [tr]
  (if-let [cause (ex-cause tr)]
    (recur cause)
    tr))

(defn print-trace-element
  "Prints a Clojure-oriented view of one element in a stack trace.
  PORT: there are no stack trace elements; prints e."
  [e]
  (print e))

(defn print-throwable
  "Prints the class and message of a Throwable. Prints the ex-data map
  if present."
  [tr]
  (printf "%s: %s" (pr-str (type tr)) (ex-message tr))
  (when-let [info (ex-data tr)]
    (newline)
    (pr info)))

(defn print-stack-trace
  "Prints a Clojure-oriented stack trace of tr, a Throwable.
  Prints a maximum of n stack frames (default: unlimited).
  Does not print chained exceptions (causes)."
  ([tr] (print-stack-trace tr nil))
  ([tr n]
   (print-throwable tr)
   (newline)
   (print " at ")
   (print "[empty stack trace]")
   (newline)))

(defn print-cause-trace
  "Like print-stack-trace but prints chained exceptions (causes)."
  ([tr] (print-cause-trace tr nil))
  ([tr n]
   (print-stack-trace tr n)
   (when-let [cause (ex-cause tr)]
     (print "Caused by: " )
     (recur cause n))))

(defn e
  "REPL utility.  Prints a brief stack trace for the root cause of the
  most recent exception."
  []
  (print-stack-trace (root-cause *e) 8))
