;;;; N-Queens, translated from the idiomatic Grail version
;;;; (grail-compat/benchmarks/nqueens/idiomatic.py).
;;;;
;;;; Every placement of N non-attacking queens, each a vector where
;;;; (cols row) is the queen's column, in lexicographic order.
;;;; Backtracking with three bitmasks (columns and both diagonals), one
;;;; search per first-row column, results concatenated in column order.
;;;; Single-threaded (the Python `threads=1` mode).
;;;;
;;;; Run: corten --image clj.img --load bench/grail/nqueens.cljc

(ns bench.nqueens)

(defn- bit-index
  "Index of the single set bit in `bit` (Python's bit.bit_length() - 1)."
  [bit]
  (loop [b bit i 0]
    (if (= b 1) i (recur (bit-shift-right b 1) (inc i)))))

(defn- extend-sols
  "Place queens in rows row..n-1, adding each full solution to the
  transient vector `out`; returns `out`."
  [n row cols diag1 diag2 placed out]
  (if (= row n)
    (conj! out placed)
    (let [full (dec (bit-shift-left 1 n))]
      (loop [free (bit-and full (bit-not (bit-or cols diag1 diag2)))
             out out]
        (if (zero? free)
          out
          (let [bit (bit-and free (- free))]   ; lowest free column
            (recur (bit-xor free bit)
                   (extend-sols n (inc row)
                                (bit-or cols bit)
                                (bit-and (bit-shift-left (bit-or diag1 bit) 1) full)
                                (bit-shift-right (bit-or diag2 bit) 1)
                                (conj placed (bit-index bit))
                                out))))))))

(defn solutions-from
  "All solutions whose queen in row 0 is in `first-col`, in lex order."
  [n first-col]
  (let [bit (bit-shift-left 1 first-col)]
    (persistent!
     (extend-sols n 1 bit (bit-and (bit-shift-left bit 1) (dec (bit-shift-left 1 n)))
                  (bit-shift-right bit 1) [first-col] (transient [])))))

(defn n-queens [n]
  (into [] (mapcat #(solutions-from n %)) (range n)))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [n reps inner] :or {n 8 reps 5 inner 100}}]
  (let [sols (n-queens n)]
    (println (count sols) "solutions; first" (first sols) "last" (peek sols)))
  ;; One solve is short, so time `inner` solves and report per solve.
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (dotimes [_ inner] (n-queens n))
      (println "nqueens:" (/ (- (now-ms) t0) inner) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
