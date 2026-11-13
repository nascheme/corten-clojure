;;;; N-body (Computer Language Benchmarks Game), translated from the
;;;; idiomatic Grail version (grail-compat/benchmarks/nbody/idiomatic.py).
;;;;
;;;; Vec3 and Body are records; the state is a vector of Body values and
;;;; `step` is a pure function from one state to the next.  Velocities
;;;; being accumulated within a step live in a local vector.  Arithmetic is
;;;; component-wise in the same order as the original.  Single-threaded,
;;;; like the Python `main`.
;;;;
;;;; Run: corten --image clj.img --load bench/grail/nbody.cljc

(ns bench.nbody)

(defrecord Vec3 [x y z])
(defrecord Body [name pos vel mass])

(defn v+ [a b] (->Vec3 (+ (:x a) (:x b)) (+ (:y a) (:y b)) (+ (:z a) (:z b))))
(defn v- [a b] (->Vec3 (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))
(defn v* [a k] (->Vec3 (* (:x a) k) (* (:y a) k) (* (:z a) k)))
(defn dot [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b))))

(def default-iterations 20000)
(def dt 0.01)
(def pi 3.14159265358979323)
(def solar-mass (* 4 pi pi))
(def days-per-year 365.24)

(defn planet [name [x y z] vel-per-day mass-in-suns]
  (->Body name (->Vec3 x y z)
          (apply ->Vec3 (map #(* % days-per-year) vel-per-day))
          (* mass-in-suns solar-mass)))

(def initial
  [(->Body "sun" (->Vec3 0.0 0.0 0.0) (->Vec3 0.0 0.0 0.0) solar-mass)
   (planet "jupiter"
           [4.84143144246472090e+00 -1.16032004402742839e+00 -1.03622044471123109e-01]
           [1.66007664274403694e-03 7.69901118419740425e-03 -6.90460016972063023e-05]
           9.54791938424326609e-04)
   (planet "saturn"
           [8.34336671824457987e+00 4.12479856412430479e+00 -4.03523417114321381e-01]
           [-2.76742510726862411e-03 4.99852801234917238e-03 2.30417297573763929e-05]
           2.85885980666130812e-04)
   (planet "uranus"
           [1.28943695621391310e+01 -1.51111514016986312e+01 -2.23307578892655734e-01]
           [2.96460137564761618e-03 2.37847173959480950e-03 -2.96589568540237556e-05]
           4.36624404335156298e-05)
   (planet "neptune"
           [1.53796971148509165e+01 -2.59193146099879641e+01 1.79258772950371181e-01]
           [2.68067772490389322e-03 1.62824170038242295e-03 -9.51592254519715870e-05]
           5.15138902046611451e-05)])

;; All unordered pairs (i < j), in itertools.combinations order.
(def pairs
  (vec (for [i (range (count initial)) j (range (inc i) (count initial))] [i j])))

(defn offset-momentum
  "Give the reference body the velocity that makes total momentum zero."
  [bodies reference]
  (let [[px py pz] (reduce (fn [[px py pz] {:keys [vel mass]}]
                             [(- px (* (:x vel) mass))
                              (- py (* (:y vel) mass))
                              (- pz (* (:z vel) mass))])
                           [0.0 0.0 0.0] bodies)]
    (mapv (fn [b]
            (if (= (:name b) reference)
              (let [m (:mass b)]
                (assoc b :vel (->Vec3 (/ px m) (/ py m) (/ pz m))))
              b))
          bodies)))

(defn energy
  "Total energy: pairwise potential, then kinetic (original's order)."
  [bodies]
  (let [e (reduce (fn [e [i j]]
                    (let [a (bodies i) b (bodies j)
                          d (v- (:pos a) (:pos b))]
                      (- e (/ (* (:mass a) (:mass b)) (Math/sqrt (dot d d))))))
                  0.0 pairs)]
    (reduce (fn [e b] (+ e (/ (* (:mass b) (dot (:vel b) (:vel b))) 2.0)))
            e bodies)))

(defn step
  "One symplectic-Euler step; returns the next state."
  [bodies dt]
  (let [vel (reduce (fn [vel [i j]]
                      (let [a (bodies i) b (bodies j)
                            d (v- (:pos a) (:pos b))
                            mag (* dt (Math/pow (dot d d) -1.5))]
                        (-> vel
                            (assoc i (v- (vel i) (v* d (* (:mass b) mag))))
                            (assoc j (v+ (vel j) (v* d (* (:mass a) mag)))))))
                    (mapv :vel bodies) pairs)]
    (mapv (fn [b v] (assoc b :pos (v+ (:pos b) (v* v dt)) :vel v))
          bodies vel)))

(defn simulate [bodies dt n]
  (loop [bodies bodies n n]
    (if (pos? n) (recur (step bodies dt) (dec n)) bodies)))

(defn run
  "One benchmark loop: energy, advance, energy.  Returns [e0 e1 state]."
  ([] (run default-iterations "sun"))
  ([iterations reference]
   (let [bodies (offset-momentum initial reference)
         e0 (energy bodies)
         bodies (simulate bodies dt iterations)]
     [e0 (energy bodies) bodies])))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (let [[e0 e1 final] (run)]
    (println "energy before:" e0)
    (println "energy after: " e1)
    (println "sun position: " (:pos (final 0))))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (run)
      (println "nbody:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
