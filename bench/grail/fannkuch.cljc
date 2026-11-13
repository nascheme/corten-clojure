;;;; Fannkuch(n), translated from the idiomatic Grail version
;;;; (grail-compat/benchmarks/fannkuch/idiomatic.py).
;;;;
;;;; Maximum pancake flips over all permutations of 0..n-1 (30 for n=9),
;;;; plus the Benchmarks Game checksum.  The permutation index range is
;;;; split into chunks (4 per thread, as in Python); each chunk decodes its
;;;; first permutation with the factorial number system, then steps
;;;; through the rest with the original's rotation rule.  Permutations
;;;; and rotation counts are local long arrays (Python: local lists).  Single-threaded (the
;;;; Python `threads=1` mode, so 4 chunks).
;;;;
;;;; Run: corten --image clj.img --load bench/grail/fannkuch.cljc

(ns bench.fannkuch)

(def default-arg 9)

(defrecord Chunk [n start stop])
(defrecord Result [max-flips checksum])

(defn combine [a b]
  (->Result (max (:max-flips a) (:max-flips b)) (+ (:checksum a) (:checksum b))))

(defn factorial [n] (reduce * 1 (range 1 (inc n))))

;; perm and count are local scratch arrays, as the Python uses local
;; lists.

(defn- rotate-left!
  "Rotate perm[0..i] left by d in place."
  [^longs perm i d]
  (let [head (vec (take d perm))]
    (dotimes [j (- (inc i) d)] (aset perm j (aget perm (+ j d))))
    (dotimes [j d] (aset perm (+ (- (inc i) d) j) (long (head j))))))

(defn count-flips
  "Pancake flips until 0 is in front, on a private copy of perm1."
  [^longs perm1]
  (let [^longs perm (aclone perm1)]
    (loop [flips 0]
      (let [k (aget perm 0)]
        (if (zero? k)
          flips
          (do (loop [i 0 j k]                 ; reverse perm[0..k]
                (when (< i j)
                  (let [t (aget perm i)]
                    (aset perm i (aget perm j))
                    (aset perm j t)
                    (recur (inc i) (dec j)))))
              (recur (inc flips))))))))

(defn run-chunk [{:keys [n start stop]}]
  (let [m (dec n)
        perm1 (long-array (range n))
        count (long-array n 0)]
    ;; Decode permutation number `start`: count[i] is the number of left
    ;; rotations of perm1[0..i] already done.
    (loop [i (dec n) idx start]
      (when (pos? i)
        (let [f (factorial i) d (quot idx f)]
          (aset count i (long d))
          (rotate-left! perm1 i d)
          (recur (dec i) (rem idx f)))))
    (loop [index start max-flips 0 checksum 0]
      (if (< index stop)
        (let [evaluate (and (not= (aget perm1 0) 0) (not= (aget perm1 m) m))
              flips (if evaluate (count-flips perm1) 0)]
          ;; Next permutation: rotate perm1[0..r] left by one, for the
          ;; smallest r whose rotation count has not wrapped.
          (loop [r 1]
            (when (< r n)
              (let [p0 (aget perm1 0)]
                (dotimes [j r] (aset perm1 j (aget perm1 (inc j))))
                (aset perm1 r p0))
              (let [c (inc (aget count r))]
                (if (<= c r)
                  (aset count r c)
                  (do (aset count r 0) (recur (inc r)))))))
          (recur (inc index)
                 (if evaluate (max flips max-flips) max-flips)
                 (if evaluate (+ checksum (if (odd? index) (- flips) flips)) checksum)))
        (->Result max-flips checksum)))))

(defn chunks-of [n pieces]
  (let [total (factorial n)
        pieces (max 1 (min pieces total))
        bounds (map #(quot (* total %) pieces) (range (inc pieces)))]
    (map #(->Chunk n %1 %2) bounds (rest bounds))))

(defn fannkuch
  ([n] (fannkuch n 1))
  ([n threads]
   (reduce combine (->Result 0 0) (map run-chunk (chunks-of n (* 4 threads))))))

(defn now-ms []
  #?(:corten (/ (corten.kernel/get-internal-real-time) 1000.0)
     :clj (/ (System/nanoTime) 1e6)))

(defn -main [& {:keys [reps] :or {reps 5}}]
  (let [res (fannkuch default-arg)]
    (println (str "Pfannkuchen(" default-arg ") = " (:max-flips res)
                  "; checksum " (:checksum res))))
  (dotimes [_ reps]
    (let [t0 (now-ms)]
      (fannkuch default-arg)
      (println "fannkuch:" (- (now-ms) t0) "ms"))))

(apply -main (when-let [[n] (seq *command-line-args*)] [:reps (parse-long n)]))
