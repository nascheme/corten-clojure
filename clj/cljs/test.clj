;;;; A small cljs.test: deftest, testing, is (with thrown? and
;;;; thrown-with-msg?), are, run-tests, plus run-file, which evaluates
;;;; a file form by form and keeps going after errors. Enough to run the
;;;; ClojureScript test files (see scripts/cljs-tests.sh). report and
;;;; assert-expr are multimethods as in clojure.test, and vars with
;;;; :test metadata (test.check's defspec) are run as tests, so
;;;; clojure.test.check.clojure-test works.

(ns cljs.test
  (:require [corten.kernel :as k]))

(def report-counters (atom {:test 0 :pass 0 :fail 0 :error 0 :load-error 0}))
(def registered-tests (atom []))
(def ^:dynamic *testing-contexts* ())
(def ^:dynamic *current-test* nil)
;; When true, run-file prints each form's label before evaluating it.
(def ^:dynamic *trace* false)
;; cljs.test's test environment; only :testing-vars is read (test.check).
(def ^:dynamic *current-env* nil)

(defn get-current-env [] *current-env*)

(defn inc-report-counter! [k]
  (swap! report-counters update k inc))

(defn- context-str []
  (apply str (interpose " " (reverse *testing-contexts*))))

(defn- err-str [e]
  (or (ex-message e) (pr-str e)))

(defn testing-vars-str [m] (str *current-test*))

(defn file-and-line
  "No stack traces here: the file and line are unknown."
  [_exception _depth]
  {:file nil :line nil})

(defn- default-report [m]
  (case (:type m)
    :pass (inc-report-counter! :pass)
    :fail (do (inc-report-counter! :fail)
              (println "\nFAIL in" (str *current-test*) (context-str))
              (when (:message m) (println (:message m)))
              (println "expected:" (pr-str (:expected m)))
              (println "  actual:" (pr-str (:actual m))))
    :error (do (inc-report-counter! :error)
               (println "\nERROR in" (str *current-test*) (context-str))
               (when (:message m) (println (:message m)))
               (println "expected:" (pr-str (:expected m)))
               (println "  actual:" (err-str (:actual m))))
    nil))

;; Dispatch on [reporter type] as cljs.test; the reporter is always
;; ::default.
(defmulti report (fn [m] [::default (:type m)]))
(defmethod report :default [m] (default-report m))

(defn do-report [m] (report m))

(defmacro testing [string & body]
  `(binding [*testing-contexts* (conj *testing-contexts* ~string)]
     ~@body))

(defmulti assert-expr
  "As in clojure.test: the code for (is form msg), by the head symbol of
  form as written (:always-fail for nil, :default otherwise)."
  (fn [msg form]
    (cond (nil? form) :always-fail
          (seq? form) (first form)
          :else :default)))

(defmethod assert-expr :always-fail [msg form]
  `(do-report {:type :fail :message ~msg}))

(defmethod assert-expr 'thrown? [msg form]
  (let [[_ klass & body] form]
    `(try ~@body
          (do-report {:type :fail :message ~msg :expected '~form :actual nil})
          (catch ~klass e#
            (do-report {:type :pass :message ~msg :expected '~form :actual e#})
            e#))))

(defmethod assert-expr 'thrown-with-msg? [msg form]
  (let [[_ klass re & body] form]
    `(try ~@body
          (do-report {:type :fail :message ~msg :expected '~form :actual nil})
          (catch ~klass e#
            (if (re-find ~re (ex-message e#))
              (do-report {:type :pass :message ~msg :expected '~form :actual e#})
              (do-report {:type :fail :message ~msg :expected '~form :actual e#}))
            e#))))

;; test.check's assertion (its own assert-expr method needs the
;; cljs.test$macros namespace).
(defmethod assert-expr 'clojure.test.check.clojure-test/check? [msg form]
  `(clojure.test.check.clojure-test.assertions/check-results ~(second form)))

;; Evaluate the arguments of =, so a failure shows their values.
(defmethod assert-expr '= [msg form]
  `(let [args# (list ~@(rest form))
         result# (apply = args#)]
     (if result#
       (do-report {:type :pass :message ~msg :expected '~form :actual result#})
       (do-report {:type :fail :message ~msg :expected '~form
                   :actual (list (symbol "not") (cons (symbol "=") args#))}))
     result#))

(defmethod assert-expr :default [msg form]
  `(let [value# ~form]
     (if value#
       (do-report {:type :pass :message ~msg :expected '~form :actual value#})
       (do-report {:type :fail :message ~msg :expected '~form :actual value#}))
     value#))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg]
   `(try ~(assert-expr msg form)
         (catch :default t#
           (do-report {:type :error :message ~msg :expected '~form :actual t#})))))

(defn- replace-syms [smap x]
  (cond
    (symbol? x) (if (contains? smap x) (get smap x) x)
    (seq? x) (apply list (map #(replace-syms smap %) x))
    (map-entry? x) (vec (map #(replace-syms smap %) x))
    (vector? x) (vec (map #(replace-syms smap %) x))
    (map? x) (into (empty x) (map #(replace-syms smap %) x))
    (set? x) (into (empty x) (map #(replace-syms smap %) x))
    :else x))

(defmacro are [argv expr & args]
  (if (or (and (empty? argv) (empty? args))
          (and (pos? (count argv))
               (pos? (count args))
               (zero? (mod (count args) (count argv)))))
    `(do ~@(map (fn [a] `(is ~(replace-syms (zipmap argv a) expr)))
                (partition (count argv) args)))
    (throw (js/Error. "The number of args doesn't match are's argv."))))

(defn register-test! [name f]
  (swap! registered-tests conj [name f]))

(defmacro deftest [name & body]
  `(do (defn ~name []
         (binding [*current-test* '~name]
           ~@body))
       (register-test! '~name ~name)
       (var ~name)))

(def fixtures (atom {:once [] :each []}))

(defn use-fixtures
  "Fixtures (fns of a test fn) run around each test (:each) or around
  the whole run (:once)."
  [kind & fs]
  (swap! fixtures assoc kind (vec fs)))

(defn- wrap-fixtures [fs f]
  (reduce (fn [g fixture] (fn [] (fixture g))) f (reverse fs)))

(defn test-one [name f]
  (inc-report-counter! :test)
  (when *trace* (println ";; test" name) (k/flush))
  (binding [*current-test* name]
    (try ((wrap-fixtures (:each @fixtures) f))
         (catch :default e
           (do-report {:type :error :message "Uncaught exception, not in assertion."
                       :expected nil :actual e})))))

(defn run-tests
  "Run the registered tests (all, or those whose names are in the set
  only) and print a summary."
  ([] (run-tests nil))
  ([only]
   ((wrap-fixtures (:once @fixtures)
                   (fn []
                     (doseq [[name f] @registered-tests]
                       (when (or (nil? only) (contains? only name))
                         (test-one name f))))))
   (let [{:keys [test pass fail error load-error]} @report-counters]
     (println "\nRan" test "tests containing" (+ pass fail error) "assertions.")
     (println fail "failures," error "errors," load-error "load errors."))
   @report-counters))

(defn- form-label [form]
  (if (seq? form)
    (pr-str (take 2 form))
    (pr-str form)))

(defn- eval-ns-form
  "Evaluate (ns name clauses*) leniently: the :refer-clojure clause, then
  each required lib and each import on its own (one that is missing is
  reported and skipped). Other clauses than :require, :require-macros,
  :use, :use-macros and :import are ignored."
  [[_ name & clauses]]
  (let [clauses (filter seq? clauses)]
    (eval (apply list 'ns name (filter #(= :refer-clojure (first %)) clauses)))
    (doseq [c clauses
            :when (contains? #{:require :require-macros :use :use-macros :import} (first c))
            spec (rest c)]
      (try (eval (list (case (first c)
                         :require 'require
                         :require-macros 'require-macros
                         :use 'use
                         ;; (:use-macros lib): (:require-macros [lib :refer :all]).
                         :use-macros 'use
                         :import 'import)
                       ;; import is a macro.
                       (if (= :import (first c)) spec (list 'quote spec))))
           (catch :default e
             (println "SKIPPED" (subs (str (first c)) 1) (pr-str spec) "-" (err-str e)))))))

(defn run-file
  "Evaluate the forms of the file at path one at a time in the current
  namespace, reporting and counting errors instead of stopping. Forms
  whose head is in skip (a set of symbols) are not evaluated; an ns form
  is evaluated by eval-ns-form."
  ([path] (run-file path #{}))
  ([path skip]
   (let [s (k/open-input-file path)
         eof (js-obj)]
     (loop []
       (let [form (try (k/read s nil eof)
                       (catch :default e
                         (println "READ ERROR:" (err-str e))
                         eof))]
         (when-not (identical? form eof)
           (when-not (and (seq? form) (contains? skip (first form)))
             (when *trace* (println ";;" (form-label form)))
             (try (if (and (seq? form) (= 'ns (first form)))
                    (eval-ns-form form)
                    (let [v (eval form)]
                      ;; A var with :test metadata not defined by deftest
                      ;; (test.check's defspec) is a test.
                      (when (and (var? v) (:test (meta v))
                                 (not (some #(= (first %) (:name (meta v))) @registered-tests)))
                        (register-test! (:name (meta v)) (:test (meta v))))))
                  (catch :default e
                    (inc-report-counter! :load-error)
                    (println "LOAD ERROR in" (form-label form) "-" (err-str e)))))
           (recur))))
     (k/close s)
     nil)))

(defmacro async
  "(async done body*): done is a fn to call when the test is finished.
  Here tests run synchronously, so done does nothing."
  [done & body]
  `(let [~done (fn [] nil)] ~@body))
