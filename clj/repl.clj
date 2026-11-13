;;;; Source loading and the REPL for Corten Clojure. The kernel provides
;;;; k/read, k/eval and k/load; `corten --image clj.img` runs `repl`.

(in-ns 'clojure.core)

(defn load-file
  "Sequentially read and evaluate the set of forms contained in the file."
  [name]
  (k/load name))

(defn load-string
  "Sequentially read and evaluate the set of forms contained in the
  string, returning the value of the last one."
  [s]
  (let [in (k/string-input-stream s)
        eof (js-obj)]
    (binding [*ns* *ns*]
      (loop [v nil]
        (let [x (k/read in nil eof)]
          (if (identical? x eof)
            v
            (recur (eval x))))))))

(defn- repl-caught
  "Print the exception e caught by the REPL to standard error."
  [e]
  (k/write-string
   (str "Error: "
        (if (instance? Error e)
          (str (ex-message e)
               (when-let [d (ex-data e)] (str " " (pr-str d))))
          (str "Clojure exception: " (pr-str e)))
        "\n")
   true))

(defn repl
  "Read-eval-print loop on standard input: prints the value of each form
  read, binding *1, *2, *3 to the last values and *e to the last
  exception. Returns at end of input."
  []
  (let [eof (js-obj)]
    (binding [*ns* *ns* *1 nil *2 nil *3 nil *e nil]
      (loop []
        (print (str (ns-name *ns*) "=> "))
        (flush)
        (when-not
            (try
              (let [x (k/read nil nil eof)]
                (if (identical? x eof)
                  true
                  (let [v (eval x)]
                    (set! *3 *2)
                    (set! *2 *1)
                    (set! *1 v)
                    (prn v)
                    false)))
              (catch :default e
                (set! *e e)
                (k/clear-input)
                (repl-caught e)
                false))
          (recur))))
    (newline)
    (flush)
    nil))
