;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;;; PORT: from ClojureScript src/main/cljs/clojure/string.cljs; edits marked.

(ns clojure.string
  (:refer-clojure :exclude [replace reverse])
  (:require [goog.string :as gstring]
            [corten.kernel :as k])
  (:import [goog.string StringBuffer]))

;; PORT: trim, triml, trimr and blank? use Java's whitespace
;; (Character/isWhitespace). Argument checks as in Clojure, where s is a CharSequence:
;; functions that call s's .toString take any non-nil value, the others
;; a string.
(defn- to-str [s]
  (if (nil? s)
    (throw (TypeError. "clojure.string: argument is nil"))
    (str s)))

(defn- check-str
  "s, a string or a StringBuilder (Clojure's CharSequence), as a string."
  [s]
  (cond
    (string? s) s
    (instance? StringBuilder s) (.toString s)
    :else (throw (TypeError. (str "clojure.string: not a string: " (pr-str s))))))

(defn- char-seq [x]
  (if (instance? StringBuilder x) (.toString x) x))

(defn- seq-reverse
  [coll]
  (reduce conj () coll))

;; PORT: re-surrogate-pair removed (see reverse).

(defn ^string reverse
  "Returns s with its characters reversed."
  [s]
  ;; PORT: strings are of code points (no surrogate pairs).
  (k/reverse (check-str s)))

(defn- replace-all
  [s re replacement]
  (let [r (js/RegExp. (.-source re)
                      (cond-> "g"
                        (.-ignoreCase re) (str "i")
                        (.-multiline re) (str "m")
                        (.-unicode re) (str "u")))]
    (corten.string/replace s r replacement)))

(defn- replace-with
  [f]
  (fn [& args]
    ;; PORT: the host passes the match and groups only (JavaScript also
    ;; passes the offset and the string).
    (let [matches args]
      (check-str (if (= (count matches) 1)
                   (f (first matches))
                   (f (vec matches)))))))

;; PORT: a pattern's string replacement has Java's syntax (Matcher's
;; appendReplacement): $n is group n, \x is x; other replacements are
;; literal.
(defn- java-replacement
  [r]
  (let [n (count r)
        digit (fn [c] (let [d (- (int c) (int \0))] (when (<= 0 d 9) d)))]
    (fn [& groups]
      (let [ng (count groups)]
        (loop [i 0 out (StringBuffer.)]
          (if (< i n)
            (let [c (nth r i)]
              (cond
                (= c \\)
                (if (< (inc i) n)
                  (recur (+ i 2) (.append out (nth r (inc i))))
                  (throw (ArgumentError. "character to be escaped is missing")))

                (= c \$)
                (let [d (when (< (inc i) n) (digit (nth r (inc i))))]
                  (when-not d
                    (throw (ArgumentError. "Illegal group reference")))
                  (when-not (< d ng)
                    (throw (IndexError. (str "No group " d))))
                  ;; Further digits while the group number stays valid.
                  (let [[g j] (loop [g d j (+ i 2)]
                                (let [e (when (< j n) (digit (nth r j)))
                                      g2 (when e (+ (* g 10) e))]
                                  (if (and g2 (< g2 ng)) (recur g2 (inc j)) [g j])))]
                    (recur j (.append out (or (nth groups g) "")))))

                :else (recur (inc i) (.append out c))))
            (.toString out)))))))

(defn ^string re-quote-replacement
  "Given a replacement string that you wish to be a literal
   replacement for a pattern match in replace or replace-first, do the
   necessary escaping of special characters in the replacement."
  [replacement]
  (let [r (to-str replacement)]
    (apply str (map (fn [c] (if (or (= c \\) (= c \$)) (str \\ c) c)) r))))

(defn ^string replace
  "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   pattern / (string or function of match).

   See also replace-first.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.

   Example:
   (clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
   -> \"lmostAay igPay atinLay\""
  [s match replacement]
  (let [s (to-str s)
        match (char-seq match)
        replacement (char-seq replacement)]
   (cond
    ;; PORT: characters are not strings.
    (char? match)
    (if (char? replacement)
      (replace s (str match) (str replacement))
      (throw (TypeError. "clojure.string/replace: char match needs a char replacement")))

    (string? match)
    (let [r (check-str replacement)]
      (corten.string/replace s (js/RegExp. (gstring/regExpEscape match) "g")
                             (fn [& _] r)))

    (instance? js/RegExp match)
    (if (string? replacement)
      (replace-all s match (java-replacement replacement))
      (replace-all s match (replace-with replacement)))

    :else (throw (TypeError. (str "Invalid match arg: " match))))))

(defn ^string replace-first
  "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   string / string
   pattern / (string or function of match).

   See also replace.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.

   Example:
   (clojure.string/replace-first \"swap first two words\"
                                 #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")
   -> \"first swap two words\""
  [s match replacement]
  (let [s (to-str s)
        match (char-seq match)
        replacement (char-seq replacement)]
    (cond
      (char? match)
      (if (char? replacement)
        (corten.string/replace s (str match) (str replacement))
        (throw (TypeError. "clojure.string/replace-first: char match needs a char replacement")))

      (string? match)
      (let [r (check-str replacement)]
        (corten.string/replace s match (fn [& _] r)))

      (instance? js/RegExp match)
      (corten.string/replace s match (if (string? replacement)
                                       (java-replacement replacement)
                                       (replace-with replacement)))

      :else (throw (TypeError. (str "Invalid match arg: " match))))))

(defn join
  "Returns a string of all elements in coll, as returned by (seq coll),
  separated by an optional separator."
  ([coll]
   (loop [sb (StringBuffer.) coll (seq coll)]
     (if-not (nil? coll)
       (recur (. sb (append (str (first coll)))) (next coll))
       ^string (.toString sb))))
  ([separator coll]
   (loop [sb (StringBuffer.) coll (seq coll)]
     (if-not (nil? coll)
       (do
         (. sb (append (str (first coll))))
         (let [coll (next coll)]
           (when-not (nil? coll)
             (. sb (append separator)))
           (recur sb coll)))
       ^string (.toString sb)))))

(defn ^string upper-case
  "Converts string to all upper-case."
  [s]
  (corten.string/upper-case (to-str s)))

(defn ^string lower-case
  "Converts string to all lower-case."
  [s]
  (corten.string/lower-case (to-str s)))

(defn ^string capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  [s]
  (gstring/capitalize (to-str s)))

;; The JavaScript split function takes a limit argument but the return
;; value is not the same as the Java split function.
;;
;; Java: (.split "a-b-c" #"-" 2) => ["a" "b-c"]
;; JavaScript: (.split "a-b-c" #"-" 2) => ["a" "b"]
;;
;; For consistency, the three arg version has been implemented to
;; mimic Java's behavior.

(defn- pop-last-while-empty
  [v]
  (loop [v v]
    (if (identical? "" (peek v))
      (recur (pop v))
      v)))

(defn- discard-trailing-if-needed
  [limit v]
  (if (and (== 0 limit) (< 1 (count v)))
    (pop-last-while-empty v)
    v))

;; PORT: as Java's String.split with an empty regex: no leading empty
;; string, a trailing one unless the limit is 0.
(defn- split-with-empty-regex
  [s limit]
  (let [n (count s)]
    (cond
      (zero? n) [""]
      (zero? limit) (mapv str s)
      (or (neg? limit) (> limit n)) (conj (mapv str s) "")
      :else (conj (mapv str (subs s 0 (dec limit))) (subs s (dec limit))))))

(defn split
  "Splits string on a regular expression. Optional argument limit is
  the maximum number of parts. Not lazy. Returns vector of the parts.
  Trailing empty strings are not returned - pass limit of -1 to return all."
  ([s re]
     (split s re 0))
    ([s re limit]
     (check-str s)
     (discard-trailing-if-needed limit
       (if (= "" (.-source re)) ;; PORT: the empty regex
         (split-with-empty-regex s limit)
         (if (< limit 1)
           (vec (corten.string/split (str s) re))
           (loop [s s
                  limit limit
                  parts []]
             (if (== 1 limit)
               (conj parts s)
               (let [m (re-find re s)]
                 (if-not (nil? m)
                   (let [index (corten.string/index-of s m)]
                     (recur (corten.string/substring s (+ index (count m)))
                       (dec limit)
                       (conj parts (corten.string/substring s 0 index))))
                   (conj parts s))))))))))

(defn split-lines
  "Splits s on \\n or \\r\\n. Trailing empty lines are not returned."
  [s]
  (split (check-str s) #"\n|\r\n"))

(defn ^string trim
  "Removes whitespace from both ends of string."
  [s]
  (k/string-trim (check-str s) 3))

(defn ^string triml
  "Removes whitespace from the left side of string."
  [s]
  (k/string-trim (check-str s) 4))

(defn ^string trimr
  "Removes whitespace from the right side of string."
  [s]
  (k/string-trim (check-str s) 5))

(defn ^string trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  [s]
  (loop [index (corten.string/length (check-str s))]
    (if (zero? index)
      ""
      (let [ch (get s (dec index))]
        (if (or (identical? \newline ch)
                (identical? \return ch))
          (recur (dec index))
          (corten.string/substring s 0 index))))))

(defn ^boolean blank?
  "True if s is nil, empty, or contains only whitespace."
  [s]
  (or (nil? s) (== 0 (count (k/string-trim (check-str s) 3)))))

(defn ^string escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:

   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
  [s cmap]
  (let [s (check-str s)
        buffer (StringBuffer.)
        length (corten.string/length s)]
    (loop [index 0]
      (if (== length index)
        (. buffer (toString))
        (let [ch (k/char s index) ;; PORT: a character
              replacement (cmap ch)]
          (if-not (nil? replacement)
            (.append buffer (str replacement))
            (.append buffer ch))
          (recur (inc index)))))))

(defn index-of
  "Return index of value (string or char) in s, optionally searching
  forward from from-index or nil if not found."
  ([s value] (corten.string/index-of (check-str s) value))
  ([s value from-index] (corten.string/index-of (check-str s) value from-index)))

(defn last-index-of
  "Return last index of value (string or char) in s, optionally
  searching backward from from-index or nil if not found."
  ([s value] (corten.string/last-index-of (check-str s) value))
  ([s value from-index] (corten.string/last-index-of (check-str s) value from-index)))

(defn ^boolean starts-with?
  "True if s starts with substr."
  [s substr]
  (gstring/startsWith (to-str s) (check-str substr)))

(defn ^boolean ends-with?
  "True if s ends with substr."
  [s substr]
  (gstring/endsWith (to-str s) (check-str substr)))

(defn ^boolean includes?
  "True if s includes substr."
  [s substr]
  (gstring/contains (to-str s) (check-str substr)))
