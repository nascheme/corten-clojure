;;;; clojure.test-helper of Clojure's test/clojure/test_helper.clj, ported
;;;; for scripts/clojure-tests.sh (edits marked ;; PORT:).

(ns clojure.test-helper
  (:use clojure.test))

;; PORT: no System/getProperty; the line separator is "\n".
(defn platform-newlines [s] s)

(defn temp-ns
  "Create and return a temporary ns, using clojure.core + uses"
  [& uses]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (apply clojure.core/use 'clojure.core uses)
    *ns*))

(defmacro eval-in-temp-ns [& forms]
  `(binding [*ns* *ns*]
     (in-ns (gensym))
     (clojure.core/use 'clojure.core)
     (eval
      '(do ~@forms))))

;; PORT: ex-cause for .getCause.
(defn causes
  [throwable]
  (loop [causes []
         t throwable]
    (if t (recur (conj causes t) (ex-cause t)) causes)))

;; PORT: ex-message for .getMessage, Error for Throwable.
(defmethod assert-expr 'fails-with-cause?
  [msg [_ exception-class msg-re & body :as form]]
  `(try
   ~@body
   (report {:type :fail, :message ~msg, :expected '~form, :actual nil})
   (catch Error t#
     (if (some (fn [cause#]
                 (and
                  (= ~exception-class (class cause#))
                  (re-find ~msg-re (ex-message cause#))))
               (causes t#))
       (report {:type :pass, :message ~msg,
                :expected '~form, :actual t#})
       (report {:type :fail, :message ~msg,
                :expected '~form, :actual t#})))))

;; PORT: get-field (Java reflection) is omitted.

(defn set-var-roots
  [maplike]
  (doseq [[var val] maplike]
    (alter-var-root var (fn [_] val))))

(defn with-var-roots*
  "Temporarily set var roots, run block, then put original roots back."
  [root-map f & args]
  (let [originals (doall (map (fn [[var _]] [var @var]) root-map))]
    (set-var-roots root-map)
    (try
     (apply f args)
     (finally
      (set-var-roots originals)))))

(defmacro with-var-roots
  [root-map & body]
  `(with-var-roots* ~root-map (fn [] ~@body)))

(defn exception
  "Use this function to ensure that execution of a program doesn't
  reach certain point."
  []
  (throw (new Exception "Exception which should never occur")))

;; PORT: no *err* or java.io writers; collect *print-err-fn* output.
(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [sb# (StringBuffer. "")]
     (binding [*print-err-fn* (fn [x#] (.append sb# x#))]
       ~@body
       (str sb#))))

(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(with-err-string-writer ~@body))

(defmacro should-print-err-message
  "Turn on all warning flags, and test that error message prints
   correctly for all semi-reasonable bindings of *err*."
  [msg-re form]
  `(binding [*warn-on-reflection* true]
    (is (re-matches ~msg-re (with-err-string-writer (eval-in-temp-ns ~form))))
    (is (re-matches ~msg-re (with-err-print-writer (eval-in-temp-ns ~form))))))

(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer (eval-in-temp-ns ~form)))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer (eval-in-temp-ns ~form)))))))

;; PORT: ex-cause and ex-message for .getCause and .getMessage.
(defmethod clojure.test/assert-expr 'thrown-with-cause-msg? [msg form]
  (let [klass (nth form 1)
        re (nth form 2)
        body (nthnext form 3)]
    `(try ~@body
          (do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch ~klass e#
            (let [m# (if (ex-cause e#) (ex-message (ex-cause e#)) (ex-message e#))]
              (if (re-find ~re m#)
                (do-report {:type :pass, :message ~msg,
                            :expected '~form, :actual e#})
                (do-report {:type :fail, :message ~msg,
                            :expected '~form, :actual e#})))
            e#))))
