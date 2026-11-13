;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.reader
  ;; PORT: no tools.reader; the kernel reader reads (see edn-read-string).
  (:require [goog.object :as gobject]
            [corten.kernel :as k]))

(defn ^:private zero-fill-right-and-truncate [s width]
  (cond
    (= width (count s)) s
    (< width (count s)) (subs s 0 width)
    :else
    ;; PORT: no goog.string.StringBuffer.
    (apply str s (repeat (- width (count s)) "0"))))

(defn ^:private divisible?
  [num div]
  (zero? (mod num div)))

(defn ^:private indivisible?
  [num div]
  (not (divisible? num div)))

(defn ^:private leap-year?
  [year]
  (and (divisible? year 4)
       (or (indivisible? year 100)
           (divisible? year 400))))

(def ^:private days-in-month
  (let [dim-norm [nil 31 28 31 30 31 30 31 31 30 31 30 31]
        dim-leap [nil 31 29 31 30 31 30 31 31 30 31 30 31]]
    (fn [month leap-year?]
      (get (if leap-year? dim-leap dim-norm) month))))

(def ^:private timestamp-regex #"(\d\d\d\d)(?:-(\d\d)(?:-(\d\d)(?:[T](\d\d)(?::(\d\d)(?::(\d\d)(?:[.](\d+))?)?)?)?)?)?(?:[Z]|([-+])(\d\d):(\d\d))?")

(defn ^:private parse-int [s]
  (let [n (js/parseInt s 10)]
    (if-not (js/isNaN n)
      n)))

(defn ^:private check [low n high msg]
  (when-not (<= low n high)
    (throw (js/Error. (str msg " Failed:  " low "<=" n "<=" high))))
  n)

(defn parse-and-validate-timestamp [s]
  (let [[_ years months days hours minutes seconds fraction offset-sign offset-hours offset-minutes :as v]
        (re-matches timestamp-regex s)]
    (if-not v
      (throw (js/Error. (str "Unrecognized date/time syntax: " s)))
      (let [years (parse-int years)
            months (or (parse-int months) 1)
            days (or (parse-int days) 1)
            hours (or (parse-int hours) 0)
            minutes (or (parse-int minutes) 0)
            seconds (or (parse-int seconds) 0)
            fraction (or (parse-int (zero-fill-right-and-truncate fraction 3)) 0)
            offset-sign (if (= offset-sign "-") -1 1)
            offset-hours (or (parse-int offset-hours) 0)
            offset-minutes (or (parse-int offset-minutes) 0)
            offset (* offset-sign (+ (* offset-hours 60) offset-minutes))]
        [years
         (check 1 months 12 "timestamp month field must be in range 1..12")
         (check 1 days (days-in-month months (leap-year? years)) "timestamp day field must be in range 1..last day in month")
         (check 0 hours 23 "timestamp hour field must be in range 0..23")
         (check 0 minutes 59 "timestamp minute field must be in range 0..59")
         (check 0 seconds (if (= minutes 59) 60 59) "timestamp second field must be in range 0..60")
         (check 0 fraction 999 "timestamp millisecond field must be in range 0..999")
         offset]))))

(defn ^:private utc-ms
  "PORT: milliseconds since the epoch of a UTC date (days from civil)."
  [y m d h mi s ms]
  (let [y (if (<= m 2) (dec y) y)
        era (quot (if (>= y 0) y (- y 399)) 400)
        yoe (- y (* era 400))
        doy (+ (quot (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (dec d))
        doe (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)
        days (+ (* era 146097) doe -719468)]
    (+ (* (+ (* (+ (* (+ (* days 24) h) 60) mi) 60) s) 1000) ms)))

(defn parse-timestamp
  [ts]
  (if-let [[years months days hours minutes seconds ms offset]
           (parse-and-validate-timestamp ts)]
    ;; PORT: no js/Date.UTC.
    (js/Date.
      (- (utc-ms years months days hours minutes seconds ms)
        (* offset 60 1000)))
    (throw (js/Error. (str "Unrecognized date/time syntax: " ts)))))

(defn ^:private read-date
  [s]
  (if (string? s)
    (parse-timestamp s)
    (throw (js/Error. "Instance literal expects a string for its timestamp."))))

(defn ^:private read-queue
  [elems]
  (if (vector? elems)
    (into cljs.core/PersistentQueue.EMPTY elems)
    (throw (js/Error. "Queue literal expects a vector for its elements."))))

(defn ^:private read-js
  [form]
  (cond
    (vector? form)
    (let [arr (array)]
      (doseq [x form]
        (corten.array/push! arr x))
      arr)

    (map? form)
    (let [obj (js-obj)]
      (doseq [[k v] form]
        (gobject/set obj (name k) v))
      obj)

    :else
    (throw
      (js/Error.
        (str "JS literal expects a vector or map containing "
             "only string or unqualified keyword keys")))))

(defn ^:private read-uuid
  [uuid]
  (if (string? uuid)
    ;; PORT: an invalid UUID is an error, as in Clojure.
    (or (parse-uuid uuid)
        (throw (ArgumentError. (str "Invalid UUID string: " uuid))))
    (throw (js/Error. "UUID literal expects a string as its representation."))))

(def ^:dynamic *default-data-reader-fn*
  (atom nil))

(def ^:dynamic *tag-table*
  (atom
    ;; PORT: no add-data-readers (data_readers.cljc).
    (identity
      {'inst  read-date
       'uuid  read-uuid
       'queue read-queue
       'js    read-js})))

(defn edn-read
  "PORT: the next form of the kernel input stream stream, read as edn with
  the tag readers of opts (:readers, :default); :eof at the end, or an
  error without :eof."
  [opts stream]
  (binding [*read-tag-fn*
            (fn [tag form]
              (let [t (symbol tag)
                    f (get (:readers opts) t)
                    d (:default opts)]
                (cond f (f form)
                      d (d t form)
                      :else (throw (js/Error. (str "No reader function for tag " t))))))]
    (k/read stream (not (contains? opts :eof)) (:eof opts) :edn)))

(defn- edn-read-string
  "PORT: the first form of string s, read as edn (see edn-read)."
  [opts s]
  (cond
    (nil? s) (:eof opts)
    (not (string? s)) (throw (js/Error. (str "Cannot read from " (pr-str s))))
    ;; Without :eof, nil at the end (cljs.reader).
    :else (edn-read (if (contains? opts :eof) opts (assoc opts :eof nil))
                    (k/string-input-stream s))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   Reads data in the edn format (subset of Clojure data):
   http://edn-format.org

   opts is a map as per cljs.tools.reader.edn/read"
  ([s]
   (edn-read-string
     {:readers @*tag-table*
      :default @*default-data-reader-fn*
      :eof nil} s))
  ([opts s]
   (edn-read-string
     (update (merge {:default @*default-data-reader-fn*} opts)
       :readers (fn [m] (merge @*tag-table* m))) s)))

(defn register-tag-parser!
  [tag f]
  (let [old-parser (get @*tag-table* tag)]
    (swap! *tag-table* assoc tag f)
    old-parser))

(defn deregister-tag-parser!
  [tag]
  (let [old-parser (get @*tag-table* tag)]
    (swap! *tag-table* dissoc tag)
    old-parser))

(defn register-default-tag-parser!
  [f]
  (let [old-parser @*default-data-reader-fn*]
    (swap! *default-data-reader-fn* (fn [_] f))
    old-parser))

(defn deregister-default-tag-parser!
  []
  (let [old-parser @*default-data-reader-fn*]
    (swap! *default-data-reader-fn* (fn [_] nil))
    old-parser))
