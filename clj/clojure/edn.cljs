;;;; clojure.edn: reading edn (no evaluation). PORT: on cljs.reader.

(ns clojure.edn
  "edn reading."
  (:require [cljs.reader :as reader]))

(defn read-string
  "Reads one object from the string s. Returns nil when s is nil or
  empty.

  opts is a map that can include the following keys:
  :eof - value to return on end-of-file. When not supplied, eof throws an exception.
  :readers  - a map of tag symbols to data-reader functions to be considered before default-data-readers.
  :default - A function of two args, that will, if present and no reader is found for a tag,
             be called with the tag and the value."
  ([s] (reader/read-string s))
  ([opts s]
   (cond
     (nil? s) nil
     (contains? opts :eof) (reader/read-string opts s)
     :else (let [eof (js-obj)
                 x (reader/read-string (assoc opts :eof eof) s)]
             (if (identical? x eof)
               (throw (Error. "EOF while reading"))
               x)))))

(defn read
  "Reads the next object from stream, a PushbackReader (default *in*).

  opts is a map as for read-string; without :eof, the end of stream is an
  error."
  ([] (read *in*))
  ([stream] (read {} stream))
  ([opts stream]
   (reader/edn-read
     (update (merge {:default @reader/*default-data-reader-fn*} opts)
             :readers (fn [m] (merge @reader/*tag-table* m)))
     (.-stream stream))))
