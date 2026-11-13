;;;; PORT: host namespaces standing in for the JavaScript runtime and
;;;; Google Closure Library functions that cljs.core uses (js/..., goog/,
;;;; goog.string, goog.object, goog.array, Math/...). Loaded before
;;;; core.cljs, so only special forms, macros and kernel functions are used.
;;;; JavaScript objects (src/clj/js.rs) and arrays (src/clj/array.rs) are
;;;; kernel objects.

(ns js
  (:require [corten.kernel :as k]))

;; PORT: js/Error is corten.lang/Error, the base error type.
(def Error corten.lang/Error)
(def TypeError corten.lang/Error)
(def ReferenceError corten.lang/Error)
(def RangeError corten.lang/Error)

(def COMPILED true)
(def Infinity ##Inf)
(def -Infinity ##-Inf)
(def NaN ##NaN)
(def undefined nil)
(def Number.POSITIVE_INFINITY ##Inf)
(def Number.NEGATIVE_INFINITY ##-Inf)
;; PORT: the fixnum range.
(def Number.MAX_SAFE_INTEGER 1152921504606846975)
(def Number.MIN_SAFE_INTEGER -1152921504606846976)
(def Number.MAX_VALUE 1.7976931348623157E308)
(def Number.MIN_VALUE 4.9E-324)

(defn- now-ms []
  (k/now-ms))

;; Time: milliseconds since the Unix epoch. (js/Date.) is now,
;; (js/Date. ms), (js/Date. "RFC 3339 timestamp") and (js/Date. year
;; month ...) as in JavaScript
;; (see the construct function below).
(deftype Date [ms]
  Object
  (getTime [_] ms)
  (valueOf [_] ms)
  (getUTCFullYear [_] (k/utc-part ms 5))
  (getUTCMonth [_] (k/- (k/utc-part ms 4) 1))
  (getUTCDate [_] (k/utc-part ms 3))
  (getUTCHours [_] (k/utc-part ms 2))
  (getUTCMinutes [_] (k/utc-part ms 1))
  (getUTCSeconds [_] (k/utc-part ms 0))
  (getUTCMilliseconds [_] (k/mod ms 1000)))
(set! (.-construct Date)
      (fn ([] (k/make-instance Date (now-ms)))
          ([x] (k/make-instance Date (if (string? x) (k/parse-timestamp x) x)))
          ;; PORT: (js/Date. year month day? h? m? s? ms?) is in UTC, not
          ;; local time.
          ([y mo & [d h mi s ms]]
           (k/make-instance
            Date
            (k/parse-timestamp
             (k/format-timestamp
              y (k/+ mo 1) (or d 1) (or h 0) (or mi 0) (or s 0) (or ms 0)))))))

;; Regular expressions (src/clj/regex.rs): Rust regex syntax, flags g i
;; m s. exec returns an array of the match and its groups; the index of
;; the last match is in lastMatchIndex (JavaScript puts it on the array).
(deftype RegExp [source flags ^:mutable lastIndex ^:mutable lastMatchIndex]
  Object
  (exec [this s]
    (let [g (k/>= (k/string-index-of (if flags flags "") "g") 0)
          r (k/regex-exec source flags s (if (and g lastIndex) lastIndex 0))]
      (if (k/nil? r)
        (do (when g (set! lastIndex 0)) nil)
        (let [m (k/nth r 1)]
          (set! lastMatchIndex (k/nth r 0))
          (when g (set! lastIndex (k/+ (k/nth r 0) (corten.string/length (k/aget m 0)))))
          m))))
  (test [this s] (k/boolean (.exec this s)))
  ;; As Java's Pattern.toString: the source.
  (toString [_] source))
(set! (.-optionalFields RegExp) true)

(def performance (k/js-obj "now" now-ms))



;; Static functions of Object, Number, Array and String.
(def Object
  (k/js-obj
   "construct" (fn [] (k/js-obj))
   "getPrototypeOf" (fn [x] nil)
   "keys" (fn [o] (k/okeys o))))
(defn Object.is [a b] (k/object-is a b))
(def Number
  (k/js-obj
   "isSafeInteger" (fn [x] (k/safe-integer? x))
   "MAX_VALUE" 1.7976931348623157E308
   "MIN_VALUE" 4.9E-324
   "MAX_SAFE_INTEGER" 9007199254740991
   "MIN_SAFE_INTEGER" -9007199254740991
   "POSITIVE_INFINITY" ##Inf
   "NEGATIVE_INFINITY" ##-Inf))
(def Array
  (k/js-obj "isArray" (fn [x] (k/array? x))))
;; js/String is the native string type (static fromCharCode).
(def String corten.lang/String)

(defn isNaN [x] (k/not (k/= x x)))
(defn isFinite [x]
  (and (k/number? x)
                           (k/not (k/= x ##Inf))
                           (k/not (k/= x ##-Inf))
                           (k/= x x)))
(defn parseInt
  ([s] (parseInt s 10))
  ([s radix] (k/parse-int s radix)))
(defn parseFloat [s] (k/parse-float s))

(ns goog
  (:require [corten.kernel :as k]))

(def global nil)

(defn typeOf [x]
  (cond
    (k/eq x nil) "null"
    (k/string? x) "string"
    (k/number? x) "number"
    (k/boolean? x) "boolean"
    (k/fn? x) "function"
    (k/array? x) "array"
    :else "object"))

(defn getUid [x] (k/uid x))
(defn isArrayLike [x] (k/array? x))
(defn isString [x] (k/string? x))

(ns goog.math
  (:require [corten.kernel :as k]))

;; Not provided: types with no instances, for instance? tests.
(deftype Long [])
(deftype Integer [])

(ns goog.string
  (:require [corten.kernel :as k]))

;; A string builder: (StringBuffer.) or (StringBuffer. s). PARTS is an
;; array of the strings appended, joined by toString.
(deftype StringBuffer [^:mutable parts]
  Object
  (append [this x]
    (if (k/array? parts)
      (corten.array/push! parts (k/str x))
      (set! parts (k/array (if parts (k/str parts) "") (k/str x))))
    this)
  (toString [_]
    (if (k/array? parts)
      (let [s (corten.array/join parts)]
        (set! parts (k/array s))
        s)
      (if parts (k/str parts) "")))
  (getLength [this] (k/length (.toString this)))
  (clear [_] (set! parts nil)))
(set! (.-optionalFields StringBuffer) true)

(defn make-buffer [s] (StringBuffer. s))

(defn isEmpty [s] (k/= 0 (k/length s)))
;; As goog.string: a suffix with no length (not a string) does not match.
(defn endsWith [s suffix]
  (if (k/string? suffix) (corten.string/ends-with? s suffix) false))
(defn startsWith [s prefix] (corten.string/starts-with? s prefix))
(defn contains [s sub] (k/>= (k/string-index-of s sub) 0))

;; JavaScript's String.prototype.trim whitespace (ASCII and no-break space).
(defn trim [s] (k/string-trim s 0))
(defn trimLeft [s] (k/string-trim s 1))
(defn trimRight [s] (k/string-trim s 2))
(defn isNumeric
  "True if s (a string or character) has only the digits 0-9."
  [s]
  (let [s (if (k/char? s) (k/string-concat s) s)
        n (k/length s)]
    (loop [i 0]
      (if (k/< i n)
        (let [c (k/char-int (k/char s i))]
          (if (k/<= 48 c 57) (recur (k/+ i 1)) false))
        true))))
(defn isEmptyOrWhitespace [s] (k/= 0 (k/length (trim s))))
(defn makeSafe [s] (if (k/eq s nil) "" (k/str s)))
(defn capitalize [s]
  (if (k/= 0 (k/length s))
    s
    (k/string-concat (corten.string/upper-case (k/subseq s 0 1))
                           (corten.string/lower-case (k/subseq s 1)))))
;; Escape the characters that are special in regular expressions.
(defn regExpEscape [s]
  (let [s (k/str s)
        n (k/length s)
        out (k/array)]
    (loop [i 0]
      (when (k/< i n)
        (let [c (k/char s i)]
          (when (k/>= (k/string-index-of "\\^$.|?*+()[]{}-" (k/string-concat c)) 0)
            (corten.array/push! out "\\"))
          (corten.array/push! out (k/string-concat c)))
        (recur (k/+ i 1))))
    (corten.array/join out)))

(ns goog.object
  (:require [corten.kernel :as k]))

(defn create [& kvs]
  (k/apply k/js-obj kvs))
(defn get
  ([o k] (k/oget o k))
  ([o k not-found] (k/oget o k not-found)))
(defn set [o k v] (k/oset o k v))
(defn containsKey [o k] (k/ocontains o k))
(defn getKeys [o] (k/okeys o))
(defn clone [o] (k/oclone o))
(defn remove [o k] (k/odelete o k))

(ns goog.array
  (:require [corten.kernel :as k]))

(defn defaultCompare [a b]
  (cond (k/js-lt a b) -1
        (k/js-lt b a) 1
        :else 0))
(defn clone [a] (k/aslice a 0))
(defn stableSort [a cmp] (k/astable-sort a cmp))
(defn shuffle [a] (k/ashuffle a))

(ns Math
  (:require [corten.kernel :as k]))

(defn floor [x] (k/math-floor x))
(defn ceil [x] (k/math-ceil x))
(defn round [x] (k/math-floor (k/+ x 0.5)))
(defn abs [x] (k/math-fn "abs" x))
(defn imul [a b] (k/bit 7 a b))
(defn random [] (k/random))
(defn sqrt [x] (k/math-fn "sqrt" x))
(defn pow [x y] (k/math-pow x y))
(defn log [x] (k/math-fn "log" x))
(defn exp [x] (k/math-fn "exp" x))
(defn sin [x] (k/math-fn "sin" x))
(defn cos [x] (k/math-fn "cos" x))
(defn tan [x] (k/math-fn "tan" x))
(defn max [a b] (k/max a b))
(defn min [a b] (k/min a b))
(def LOG2E 1.4426950408889634)
(def LN2 0.6931471805599453)
(def E 2.718281828459045)
(def PI 3.141592653589793)

;; java.lang.Math statics beyond JavaScript's (see clojure.math).
(defn asin [x] (k/math1 "asin" x))
(defn acos [x] (k/math1 "acos" x))
(defn atan [x] (k/math1 "atan" x))
(defn toRadians [x] (k/math1 "to-radians" x))
(defn toDegrees [x] (k/math1 "to-degrees" x))
(defn log10 [x] (k/math1 "log10" x))
(defn cbrt [x] (k/math1 "cbrt" x))
(defn rint [x] (k/math1 "rint" x))
(defn ulp [x] (k/math1 "ulp" x))
(defn signum [x] (k/math1 "signum" x))
(defn sinh [x] (k/math1 "sinh" x))
(defn cosh [x] (k/math1 "cosh" x))
(defn tanh [x] (k/math1 "tanh" x))
(defn expm1 [x] (k/math1 "expm1" x))
(defn log1p [x] (k/math1 "log1p" x))
(defn nextUp [x] (k/math1 "next-up" x))
(defn nextDown [x] (k/math1 "next-down" x))
(defn getExponent [x] (k/math1 "get-exponent" x))
(defn atan2 [x y] (k/math2 "atan2" x y))
(defn IEEEremainder [x y] (k/math2 "IEEE-remainder" x y))
(defn hypot [x y] (k/math2 "hypot" x y))
(defn copySign [x y] (k/math2 "copy-sign" x y))
(defn nextAfter [x y] (k/math2 "next-after" x y))
(defn scalb [x y] (k/math2 "scalb" x y))
(defn incrementExact [x] (k/math-exact "increment-exact" x))
(defn decrementExact [x] (k/math-exact "decrement-exact" x))
(defn negateExact [x] (k/math-exact "negate-exact" x))
(defn addExact [x y] (k/math-exact "add-exact" x y))
(defn subtractExact [x y] (k/math-exact "subtract-exact" x y))
(defn multiplyExact [x y] (k/math-exact "multiply-exact" x y))
(defn floorDiv [x y] (k/math-exact "floor-div" x y))
(defn floorMod [x y] (k/math-exact "floor-mod" x y))

;; js/Math as a value: (.pow js/Math x y), (.-LOG2E js/Math).
(ns js
  (:require [corten.kernel :as k]))
(def Math
  (k/js-obj
   "floor" Math/floor "ceil" Math/ceil "round" Math/round "abs" Math/abs
   "imul" Math/imul "random" Math/random "sqrt" Math/sqrt "pow" Math/pow
   "log" Math/log "exp" Math/exp "sin" Math/sin "cos" Math/cos "tan" Math/tan "max" Math/max "min" Math/min
   "LOG2E" Math/LOG2E "LN2" Math/LN2 "E" Math/E "PI" Math/PI))
