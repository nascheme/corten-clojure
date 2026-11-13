;;;; raytrace: a toy Whitted-style ray tracer, translated from the
;;;; idiomatic Grail version (grail-compat/benchmarks/raytrace/idiomatic.py).
;;;;
;;;; A 100x100 RGB image of one big sphere, six small spheres and a
;;;; checkerboard floor, two point lights, reflections at most 4 deep.
;;;; Vectors, shapes, surfaces and the scene are records; the shape and
;;;; surface operations are protocols (the Python generic functions).
;;;; Python's distinct Vec and Point records are one Vec3 record here.
;;;; The original's quirks are kept (see the Python docstring).
;;;; Single-threaded (the Python `threads=1` mode).
;;;;
;;;; The image checksum is h = (h * 31 + byte) mod 2^32 over the RGB
;;;; bytes, top row first; in Python:
;;;;   h = 0
;;;;   for b in img: h = (h * 31 + b) & 0xFFFFFFFF
;;;;
;;;; Run: corten --image clj.img --load bench/grail/raytrace.cljc

(ns bench.raytrace)

(def default-width 100)
(def default-height 100)
(def epsilon 0.00001)

;;; ------------------------------------------------------------ vectors

(defrecord Vec3 [x y z])

(defn v+ [a b] (->Vec3 (+ (:x a) (:x b)) (+ (:y a) (:y b)) (+ (:z a) (:z b))))
(defn v- [a b] (->Vec3 (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))
(defn v* [a k] (->Vec3 (* k (:x a)) (* k (:y a)) (* k (:z a))))

(defn dot [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b)) (* (:z a) (:z b))))

(defn cross [a b]
  (->Vec3 (- (* (:y a) (:z b)) (* (:z a) (:y b)))
          (- (* (:z a) (:x b)) (* (:x a) (:z b)))
          (- (* (:x a) (:y b)) (* (:y a) (:x b)))))

(defn normalized [v] (v* v (/ 1.0 (Math/sqrt (dot v v)))))

(defn reflect [v normal] (v- v (v* (v* normal (dot v normal)) 2.0)))

(def up (->Vec3 0.0 1.0 0.0))
(def origin (->Vec3 0.0 0.0 0.0))

(defrecord Ray [point vector])          ; vector is unit length

(defn make-ray [point direction] (->Ray point (normalized direction)))

(defn point-at [ray t] (v+ (:point ray) (v* (:vector ray) t)))

;;; ------------------------------------------------------------- shapes

(defprotocol Shape
  (intersection-time [shape ray] "Distance along ray to the shape, or nil.")
  (normal-at [shape p] "Unit surface normal at point p."))

(defrecord Sphere [centre radius]
  Shape
  (intersection-time [_ ray]
    (let [cp (v- centre (:point ray))
          v (dot cp (:vector ray))
          discriminant (- (* radius radius) (- (dot cp cp) (* v v)))]
      (when-not (neg? discriminant)
        (- v (Math/sqrt discriminant)))))
  (normal-at [_ p] (normalized (v- p centre))))

(defrecord Halfspace [point normal]     ; normal is unit length
  Shape
  ;; The original's formula: ignores point.
  (intersection-time [_ ray]
    (let [v (dot (:vector ray) normal)]
      (when-not (zero? v) (/ 1 (- v)))))
  (normal-at [_ _] normal))

(defn make-halfspace [point normal] (->Halfspace point (normalized normal)))

;;; ----------------------------------------------------------- surfaces

(defrecord Colour [r g b])

(def black (->Colour 0.0 0.0 0.0))

(defn mix "a + k*b" [a k b]
  (->Colour (+ (:r a) (* k (:r b))) (+ (:g a) (* k (:g b))) (+ (:b a) (* k (:b b)))))

(defrecord Finish [specular lambert ambient])

(def default-finish (->Finish 0.2 0.6 (- 1.0 0.2 0.6)))

(defprotocol Surface
  (base-colour-at [surface p] "Unlit colour of the surface at point p."))

(defrecord SimpleSurface [colour finish]
  Surface
  (base-colour-at [_ _] colour))

(defrecord Checkerboard [colour other-colour check-size finish]
  Surface
  (base-colour-at [_ p]
    (let [v (v* (v- p origin) (/ 1.0 check-size))
          odd (mod (+ (long (+ (Math/abs (double (:x v))) 0.5))
                      (long (+ (Math/abs (double (:y v))) 0.5))
                      (long (+ (Math/abs (double (:z v))) 0.5)))
                   2)]
      (if (zero? odd) colour other-colour))))

;;; -------------------------------------------------------------- scene

(defrecord Thing [shape surface])
(defrecord Camera [position looking-at field-of-view])
(defrecord Scene [camera things lights])

(defn first-hit
  "[thing t] for the nearest thing hit (t > -epsilon), or nil; earlier
  things win ties."
  [scene ray]
  (reduce (fn [best thing]
            (let [t (intersection-time (:shape thing) ray)]
              (if (and t (> t (- epsilon))
                       (or (nil? best) (< t (best 1))))
                [thing t]
                best)))
          nil (:things scene)))

(defn light-visible? [scene light p]
  (let [ray (make-ray p (v- light p))]
    (not-any? (fn [thing]
                (let [t (intersection-time (:shape thing) ray)]
                  (and t (> t epsilon))))
              (:things scene))))

(declare colour-at)

(defn ray-colour [scene ray depth]
  (if (> depth 3)
    black
    (if-let [[thing t] (first-hit scene ray)]
      (let [p (point-at ray t)]
        (colour-at scene (:surface thing) ray p
                   (normal-at (:shape thing) p) depth))
      black)))

(defn colour-at [scene surface ray p normal depth]
  (let [base (base-colour-at surface p)
        {:keys [specular lambert ambient]} (:finish surface)
        c black
        c (if (pos? specular)
            (let [reflected (make-ray p (reflect (:vector ray) normal))]
              (mix c specular (ray-colour scene reflected (inc depth))))
            c)
        c (if (pos? lambert)
            (let [amount (reduce (fn [amount light]
                                   (if (light-visible? scene light p)
                                     (let [contribution (dot (normalized (v- light p)) normal)]
                                       (if (pos? contribution) (+ amount contribution) amount))
                                     amount))
                                 0.0 (:lights scene))]
              (mix c (* lambert (min 1.0 amount)) base))
            c)]
    (if (pos? ambient) (mix c ambient base) c)))

;;; ---------------------------------------------------------- rendering

(defrecord View [eye right up half-width half-height pixel-width pixel-height])

(defn make-view [cam width height]
  (let [half-w (Math/tan (/ (* Math/PI (/ (:field-of-view cam) 2.0)) 180.0))
        half-h (* 0.75 half-w)
        eye (make-ray (:position cam) (v- (:looking-at cam) (:position cam)))
        right (normalized (cross (:vector eye) up))
        up (normalized (cross right (:vector eye)))]
    (->View eye right up half-w half-h
            (/ (* half-w 2) (dec width)) (/ (* half-h 2) (dec height)))))

(defn to-byte [c] (max 0 (min 255 (long (* c 255)))))

(defn render-row
  "RGB bytes of image row y (y = 0 is the bottom row), into transient out."
  [scene view width y out]
  (let [ycomp (v* (:up view) (- (* y (:pixel-height view)) (:half-height view)))
        eye (:eye view)]
    (reduce (fn [out x]
              (let [xcomp (v* (:right view) (- (* x (:pixel-width view)) (:half-width view)))
                    ray (make-ray (:point eye) (v+ (v+ (:vector eye) xcomp) ycomp))
                    c (ray-colour scene ray 0)]
                (-> out
                    (conj! (to-byte (:r c)))
                    (conj! (to-byte (:g c)))
                    (conj! (to-byte (:b c))))))
            out (range width))))

(defn render
  "The image as a vector of width*height*3 RGB bytes, top row first."
  [scene width height]
  (let [view (make-view (:camera scene) width height)]
    (persistent!
     (reduce (fn [out y] (render-row scene view width y out))
             (transient []) (range (dec height) -1 -1)))))

(defn default-scene []
  (let [small (for [y (range 6)]
                (->Thing (->Sphere (->Vec3 (- -3 (* y 0.4)) 2.3 -5.0) 0.4)
                         (->SimpleSurface (->Colour (/ y 6.0) (- 1 (/ y 6.0)) 0.5)
                                          default-finish)))]
    (->Scene (->Camera (->Vec3 0.0 1.8 10.0) (->Vec3 0.0 3.0 0.0) 45.0)
             (vec (concat [(->Thing (->Sphere (->Vec3 1.0 3.0 -10.0) 2.0)
                                    (->SimpleSurface (->Colour 1.0 1.0 0.0) default-finish))]
                          small
                          [(->Thing (make-halfspace origin up)
                                    (->Checkerboard (->Colour 1.0 1.0 1.0) black 1.0
                                                    default-finish))]))
             [(->Vec3 30.0 30.0 10.0) (->Vec3 -10.0 100.0 30.0)])))

(defn run
  "Same workload as one loop of the original: build scene, render."
  ([] (run default-width default-height))
  ([width height] (render (default-scene) width height)))

(defn checksum [img]
  (reduce (fn [h b] (bit-and (+ (* h 31) b) 0xFFFFFFFF)) 0 img))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (let [img (run)]
    (println (count img) "bytes, checksum" (checksum img)))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (run)
      (println "raytrace:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
