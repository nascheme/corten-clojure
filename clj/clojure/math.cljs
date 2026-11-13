;;;; clojure.math: java.lang.Math's functions (src/clj/math.rs). Double
;;;; arguments may be any number; results are doubles except as noted.

(ns clojure.math
  (:require [corten.kernel :as k]))

(def ^{:doc "e, the base of natural logarithms."} E 2.718281828459045)
(def ^{:doc "pi, the ratio of a circle's circumference to its diameter."} PI 3.141592653589793)

(defn sin
  "The sine."
  [a] (k/math1 "sin" a))

(defn cos
  "The cosine."
  [a] (k/math1 "cos" a))

(defn tan
  "The tangent."
  [a] (k/math1 "tan" a))

(defn asin
  "The arc sine."
  [a] (k/math1 "asin" a))

(defn acos
  "The arc cosine."
  [a] (k/math1 "acos" a))

(defn atan
  "The arc tangent."
  [a] (k/math1 "atan" a))

(defn to-radians
  "The degrees a in radians."
  [a] (k/math1 "to-radians" a))

(defn to-degrees
  "The radians a in degrees."
  [a] (k/math1 "to-degrees" a))

(defn exp
  "The e to the power a."
  [a] (k/math1 "exp" a))

(defn log
  "The natural logarithm."
  [a] (k/math1 "log" a))

(defn log10
  "The base 10 logarithm."
  [a] (k/math1 "log10" a))

(defn sqrt
  "The positive square root."
  [a] (k/math1 "sqrt" a))

(defn cbrt
  "The cube root."
  [a] (k/math1 "cbrt" a))

(defn ceil
  "The smallest integral double >= a."
  [a] (k/math1 "ceil" a))

(defn floor
  "The largest integral double <= a."
  [a] (k/math1 "floor" a))

(defn rint
  "The nearest integral double, ties to even."
  [a] (k/math1 "rint" a))

(defn ulp
  "The size of an ulp of a."
  [a] (k/math1 "ulp" a))

(defn signum
  "The signum (-1.0, 0.0 or 1.0; zeros and NaN as is)."
  [a] (k/math1 "signum" a))

(defn sinh
  "The hyperbolic sine."
  [a] (k/math1 "sinh" a))

(defn cosh
  "The hyperbolic cosine."
  [a] (k/math1 "cosh" a))

(defn tanh
  "The hyperbolic tangent."
  [a] (k/math1 "tanh" a))

(defn expm1
  "The e^a - 1."
  [a] (k/math1 "expm1" a))

(defn log1p
  "The ln(1 + a)."
  [a] (k/math1 "log1p" a))

(defn next-up
  "The adjacent double toward positive infinity."
  [a] (k/math1 "next-up" a))

(defn next-down
  "The adjacent double toward negative infinity."
  [a] (k/math1 "next-down" a))

(defn IEEE-remainder
  "The IEEE 754 remainder."
  [dividend divisor] (k/math2 "IEEE-remainder" dividend divisor))

(defn atan2
  "The angle theta of the polar coordinates of (x, y)."
  [y x] (k/math2 "atan2" y x))

(defn pow
  "The a to the power b."
  [a b] (k/math2 "pow" a b))

(defn hypot
  "The sqrt(x^2 + y^2) without overflow."
  [x y] (k/math2 "hypot" x y))

(defn copy-sign
  "The magnitude with the sign of sign."
  [magnitude sign] (k/math2 "copy-sign" magnitude sign))

(defn next-after
  "The adjacent double to start toward direction."
  [start direction] (k/math2 "next-after" start direction))

(defn scalb
  "The d * 2^scaleFactor, rounded."
  [d scaleFactor] (k/math2 "scalb" d scaleFactor))

(defn round
  "The closest long to a, ties toward positive infinity; NaN is 0 and
  out-of-range values saturate."
  [a] (k/math1 "round" a))

(defn get-exponent
  "The unbiased exponent of d."
  [d] (k/math1 "get-exponent" d))

(defn random
  "A random double in [0.0, 1.0)."
  [] (k/random))

(defn increment-exact
  "a + 1; throws ArithmeticError on long overflow."
  [a] (k/math-exact "increment-exact" a))

(defn decrement-exact
  "a - 1; throws ArithmeticError on long overflow."
  [a] (k/math-exact "decrement-exact" a))

(defn negate-exact
  "-a; throws ArithmeticError on long overflow."
  [a] (k/math-exact "negate-exact" a))

(defn add-exact
  "x + y; throws ArithmeticError on long overflow."
  [x y] (k/math-exact "add-exact" x y))

(defn subtract-exact
  "x - y; throws ArithmeticError on long overflow."
  [x y] (k/math-exact "subtract-exact" x y))

(defn multiply-exact
  "x * y; throws ArithmeticError on long overflow."
  [x y] (k/math-exact "multiply-exact" x y))

(defn floor-div
  "The largest long <= x / y."
  [x y] (k/math-exact "floor-div" x y))

(defn floor-mod
  "x - (floor-div x y) * y, with the sign of y."
  [x y] (k/math-exact "floor-mod" x y))
