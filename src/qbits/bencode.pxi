(ns qbits.bencode
  (:require
   [pixie.streams :as st :refer [IInputStream]]
   [pixie.io :as io]
   [pixie.fs :as fs :refer [IFile]]
   [pixie.streams.utf8  :as utf8 :refer [IUTF8InputStream read-char]]))

(defprotocol IEncodable
  (-encode [x]))

(defprotocol IReadable
  (-read [x]))

(defn eox?
  [x]
  (= \e x))

(defmulti -decode (fn [token stream] token))

(defn read-chars
  [xs]
  (read-string (apply str xs)))

(defn -decode-coll
  [stream]
  (loop [val []]
    (let [char (read-char stream)]
      (if (eox? char)
        val
        (recur (conj val (-decode char stream)))))))

(defmethod -decode \d
  [_ stream]
  (apply hash-map (-decode-coll stream)))

(defmethod -decode \l
  [_ stream]
  (-decode-coll stream))

(defmethod -decode \i
  [_ stream]
  (loop [val []]
    (let [char (read-char stream)]
      (if (eox? char)
        (read-chars val)
        (recur (conj val char))))))

(defmethod -decode :default
  [token stream]
  (let [len (loop [val [token]]
              (let [char (read-char stream)]
                (if (= \: char)
                  (read-chars val)
                  (recur (conj val char)))))]
    (loop [val []
           idx 1]
      (let [char (read-char stream)]
        (if (= idx len)
          (apply str (conj val char))
          (recur (conj val char)
                 (inc idx)))))))

(extend-protocol IEncodable
  String
  (-encode [x] (str (count x)  ":" x))

  Number
  (-encode [x] (str \i x \e))

  Keyword
  (-encode [x] (-encode (name x)))

  PersistentVector
  (-encode [xs]
    (str "l" (transduce (map -encode) string-builder xs) "e"))

  MapEntry
  (-encode [[k v]]
    (str (-encode k) (-encode v)))

  IMap
  (-encode [x]
    (str "d" (transduce (map -encode) string-builder x) "e")))

(deftype UTF8StringReader [s idx len]
  IUTF8InputStream
  (read-char [this]
    (when (< idx len)
      (let [char (nth s idx)]
        (set-field! this :idx (inc idx))
        char))))

(extend-protocol IReadable
  IInputStream
  (-read [x]
    (-> x
        io/buffered-input-stream
        utf8/utf8-input-stream
        read))

  IUTF8InputStream
  (-read [x] x)

  String
  (-read [x]
    (-read (->UTF8StringReader x 0 (count x))))

  IFile
  (-read [x]
    (some-> x
            fs/abs
            io/open-read
            -read)))

(defn decode
  [x]
  (when-let [stream (-read x)]
    (when-let [char (utf8/read-char stream)]
      (-decode char stream))))

(defn encode [x]
  (-encode x))
