;;;; Float benchmark (originally from Factor), translated from the
;;;; idiomatic Grail version (grail-compat/benchmarks/float/idiomatic.py).
;;;;
;;;; For i in 0..n-1 build p_i = (sin i, 3 cos i, sin(i)^2 / 2), scale it
;;;; to unit length, and return the component-wise maximum.  Point is a
;;;; record; each point is made, normalized and folded into a running
;;;; maximum.  Single-threaded (the Python `threads=1` mode).
;;;;
;;;; Run: corten --image clj.img --load bench/grail/float.cljc

(ns bench.float)

(def points 100000)

(defrecord Point [x y z])

(defn make-point [i]
  (let [x (Math/sin i)]
    (->Point x (* (Math/cos i) 3) (/ (* x x) 2))))

(defn normalize [{:keys [x y z]}]
  (let [norm (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (->Point (/ x norm) (/ y norm) (/ z norm))))

(defn maximize
  "Component-wise maximum (same tie rule as the original)."
  [a b]
  (->Point (if (> (:x a) (:x b)) (:x a) (:x b))
           (if (> (:y a) (:y b)) (:y a) (:y b))
           (if (> (:z a) (:z b)) (:z a) (:z b))))

(defn max-normalized
  "Maximum of (normalize (make-point i)) for i in [start, stop)."
  [start stop]
  (transduce (map (comp normalize make-point))
             (completing maximize)
             (normalize (make-point start))
             (range (inc start) stop)))

(defn benchmark
  ([] (benchmark points))
  ([n] (max-normalized 0 n)))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (prn (benchmark))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (benchmark)
      (println "float:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
