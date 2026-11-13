;;;; PORT: test.check's clojure.test.check.random (random.clj, the JVM
;;;; version: SplittableRandom on 64-bit Longs) for the cljs test runs,
;;;; replacing random.cljs, whose goog.math.Long arithmetic is written
;;;; for JavaScript's 32-bit bit operations. Arithmetic is unchecked (the
;;;; original sets *unchecked-math*). rand-long returns a goog.math.Long,
;;;; as the cljs version (generators.cljc's :cljs code expects one).

; Copyright (c) Rich Hickey, Reid Draper, and contributors.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:author "Gary Fredericks"
      :doc "Purely functional and splittable pseudo-random number generators."}
 clojure.test.check.random
  (:require [goog.math.Long]))

(defprotocol IRandom
  (rand-long [rng]
    "Returns a random long based on the given immutable RNG.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument")
  (rand-double [rng]
    "Returns a random double between 0.0 (inclusive) and 1.0 (exclusive)
  based on the given immutable RNG.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument")
  (split [rng]
    "Returns two new RNGs [rng1 rng2], which should generate
  sufficiently independent random data.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument")
  (split-n [rng n]
    "Returns a collection of `n` RNGs, which should generate
  sufficiently independent random data.

  Note: to maintain independence you should not call more than one
  function in the IRandom protocol with the same argument"))

(defn- bxoubsr
  "(-> x (unsigned-bit-shift-right n) (bit-xor x))."
  [x n]
  (bit-xor (unsigned-bit-shift-right x n) x))

(defn- mix-64 [n]
  (-> n
      (bxoubsr 30)
      (unchecked-multiply -4658895280553007687)
      (bxoubsr 27)
      (unchecked-multiply -7723592293110705685)
      (bxoubsr 31)))

(defn- mix-gamma [n]
  (let [z (-> n
              (bxoubsr 33)
              (unchecked-multiply -49064778989728563)
              (bxoubsr 33)
              (unchecked-multiply -4265267296055464877)
              (bxoubsr 33)
              (bit-or 1))]
    (if (> 24 (bit-count (bxoubsr z 1)))
      (bit-xor z -6148914691236517206)
      z)))

(def ^:private double-unit (/ 1.0 (double (bit-set 0 53))))

(deftype JavaUtilSplittableRandom [gamma state]
  IRandom
  (rand-long [_]
    (goog.math/long-from-value (mix-64 (unchecked-add state gamma))))
  (rand-double [_]
    (* double-unit (unsigned-bit-shift-right (mix-64 (unchecked-add state gamma)) 11)))
  (split [this]
    (let [state' (unchecked-add gamma state)
          state'' (unchecked-add gamma state')
          gamma' (mix-gamma state'')]
      [(JavaUtilSplittableRandom. gamma state'')
       (JavaUtilSplittableRandom. gamma' (mix-64 state'))]))
  (split-n [this n]
    (case n
      0 []
      1 [this]
      (let [n-dec (dec n)]
        (loop [state state
               ret (transient [])]
          (if (= n-dec (count ret))
            (-> ret
                (conj! (JavaUtilSplittableRandom. gamma state))
                (persistent!))
            (let [state' (unchecked-add gamma state)
                  state'' (unchecked-add gamma state')
                  gamma' (mix-gamma state'')
                  new-rng (JavaUtilSplittableRandom. gamma' (mix-64 state'))]
              (recur state'' (conj! ret new-rng)))))))))

(def ^:private golden-gamma -7046029254386353131)

(defn make-java-util-splittable-random
  [seed]
  (JavaUtilSplittableRandom. golden-gamma (long seed)))

(def ^:private next-rng
  "Returns a random-number generator. Successive calls should return
  independent results."
  (let [a (atom (make-java-util-splittable-random (.valueOf (js/Date.))))]
    (fn []
      (let [[rng1 rng2] (split @a)]
        (reset! a rng2)
        rng1))))

(defn make-random
  "Given an optional Long seed, returns an object that satisfies the
  IRandom protocol."
  ([] (next-rng))
  ([seed] (make-java-util-splittable-random seed)))
