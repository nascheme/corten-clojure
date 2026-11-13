;;;; Spectral norm (Computer Language Benchmarks Game), translated from the
;;;; idiomatic Grail version
;;;; (grail-compat/benchmarks/spectral_norm/idiomatic.py).
;;;;
;;;; The spectral norm of A[i, j] = 1 / ((i+j)(i+j+1)/2 + i + 1), n x n, by
;;;; the power method on A^T A, ten steps from u = (1, ..., 1).  Vectors
;;;; are persistent vectors of doubles rebuilt by each product; every
;;;; row's sum is in the original's j order.  Single-threaded (the Python
;;;; `threads=1` mode).
;;;;
;;;; Run: corten --image clj.img --load bench/grail/spectral_norm.cljc

(ns bench.spectral-norm)

(def default-n 130)
(def power-steps 10)

(defn a
  "Entry A[i, j] of the matrix."
  [i j]
  (/ 1.0 (+ (quot (* (+ i j) (+ i j 1)) 2) i 1)))

(defn row
  "Row i of (A u), or of (A^T u) when `transposed`."
  [u i transposed]
  (reduce-kv (fn [s j u-j]
               (+ s (* (if transposed (a j i) (a i j)) u-j)))
             0.0 u))

(defn times [u transposed]
  (mapv #(row u % transposed) (range (count u))))

(defn ata-times [u]
  (times (times u false) true))

(defn spectral-norm
  ([] (spectral-norm default-n))
  ([n]
   (loop [k 0 u (vec (repeat n 1.0)) v nil]
     (if (< k power-steps)
       (let [v (ata-times u)]
         (recur (inc k) (ata-times v) v))
       (let [vbv (reduce + 0.0 (map * u v))
             vv (reduce + 0.0 (map * v v))]
         (Math/sqrt (/ vbv vv)))))))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (prn (spectral-norm))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (spectral-norm)
      (println "spectral_norm:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
