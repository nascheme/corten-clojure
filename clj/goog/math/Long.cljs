;;;; PORT: goog.math.Long of the Google Closure Library: 64-bit two's
;;;; complement integers as two signed 32-bit halves, with the arithmetic
;;;; done on native 64-bit Longs. Loaded by (:require [goog.math.Long]).
;;;; Replaces the stub type goog.math/Long of host.cljs.

(ns goog.math
  (:require [corten.kernel :as k]))

(declare long-from-bits)

(defn- long-value
  "The native Long of halves low and high."
  [low high]
  (bit-or (bit-shift-left high 32) (bit-and low 0xFFFFFFFF)))

(defn long-from-value [v]
  (long-from-bits (unchecked-int v) (unchecked-int (bit-shift-right v 32))))

(deftype Long [low_ high_]
  Object
  (toInt [_] low_)
  (toNumber [_] (double (long-value low_ high_)))
  (getLowBits [_] low_)
  (getHighBits [_] high_)
  (getLowBitsUnsigned [_] (bit-and low_ 0xFFFFFFFF))
  (isZero [_] (and (zero? low_) (zero? high_)))
  (isNegative [_] (neg? high_))
  (isOdd [_] (odd? low_))
  (equals [_ o] (and (== low_ (.-low_ o)) (== high_ (.-high_ o))))
  (notEquals [this o] (not (.equals this o)))
  (compare [this o] (compare (.value this) (.value o)))
  (lessThan [this o] (< (.value this) (.value o)))
  (lessThanOrEqual [this o] (<= (.value this) (.value o)))
  (greaterThan [this o] (> (.value this) (.value o)))
  (greaterThanOrEqual [this o] (>= (.value this) (.value o)))
  (value [_] (long-value low_ high_))
  (negate [this] (long-from-value (unchecked-negate (.value this))))
  (add [this o] (long-from-value (unchecked-add (.value this) (.value o))))
  (subtract [this o] (long-from-value (unchecked-subtract (.value this) (.value o))))
  (multiply [this o] (long-from-value (unchecked-multiply (.value this) (.value o))))
  (div [this o] (long-from-value (quot (.value this) (.value o))))
  (modulo [this o] (long-from-value (rem (.value this) (.value o))))
  (not [this] (long-from-value (bit-not (.value this))))
  (and [this o] (long-from-value (bit-and (.value this) (.value o))))
  (or [this o] (long-from-value (bit-or (.value this) (.value o))))
  (xor [this o] (long-from-value (bit-xor (.value this) (.value o))))
  (shiftLeft [this n] (long-from-value (bit-shift-left (.value this) n)))
  (shiftRight [this n] (long-from-value (bit-shift-right (.value this) n)))
  (shiftRightUnsigned [this n] (long-from-value (unsigned-bit-shift-right (.value this) n)))
  (toString [this] (.toString this 10))
  (toString [this radix] (k/integer-string (.value this) radix))
  (valueOf [this] (.toNumber this))

  IEquiv
  (-equiv [this o] (and (instance? Long o) (.equals this o)))

  IHash
  (-hash [_] (bit-xor low_ high_))

  IPrintWithWriter
  (-pr-writer [this w _] (-write w (.toString this 10))))

(defn long-from-bits [low high]
  (Long. (unchecked-int low) (unchecked-int high)))

(def long-zero (long-from-bits 0 0))
(def long-one (long-from-bits 1 0))
(def long-min (long-from-bits 0 -2147483648))
(def long-max (long-from-bits -1 2147483647))

(ns goog.math.Long
  (:require [corten.kernel :as k]))

(defn fromBits [low high] (goog.math/long-from-bits low high))
(defn fromInt [v] (goog.math/long-from-value (long v)))

(defn fromNumber [v]
  (cond (js/isNaN v) goog.math/long-zero
        (<= v -9.223372036854776e18) goog.math/long-min
        (>= v 9.223372036854776e18) goog.math/long-max
        :else (goog.math/long-from-value (long v))))

(defn fromString
  ([s] (fromString s 10))
  ([s radix]
   (when (zero? (count s))
     (throw (js/Error. "number format error: empty string")))
   (goog.math/long-from-value (k/parse-integer s radix))))

(defn getZero [] goog.math/long-zero)
(defn getOne [] goog.math/long-one)
(defn getNegOne [] (fromInt -1))
(defn getMaxValue [] goog.math/long-max)
(defn getMinValue [] goog.math/long-min)
