(ns clojure-algorithms.ciphers.base58
  "NOTE: the upstream base58.py source actually implements Base64
  (RFC 4648); this port faithfully mirrors that file body.")

(def b64-charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn- str-to-bits
  [s]
  (apply str (map #(let [b (format "%8s" (Integer/toBinaryString (int %)))]
                     (clojure.string/replace b #" " "0"))
                  (.getBytes ^String s "UTF-8"))))

(defn base64-encode
  "Encodes a string per RFC 4648."
  [data]
  (when-not (string? data)
    (throw (IllegalArgumentException. "a bytes-like object is required")))
  (let [binary (str-to-bits data)
        rem6 (mod (count binary) 6)]
    (if (zero? rem6)
      (apply str (map #(nth b64-charset (Integer/parseInt (apply str %) 2))
                      (partition 6 binary)))
      (let [pad (apply str (repeat (quot (- 6 rem6) 2) \=))
            padded (str binary (apply str (repeat (- 6 rem6) \0)))]
        (str (apply str (map #(nth b64-charset (Integer/parseInt (apply str %) 2))
                             (partition 6 padded)))
             pad)))))

(defn base64-decode
  "Decodes a Base64 string."
  [encoded-data]
  (let [s (str encoded-data)
        padding (count (filter #{\=} s))]
    (assert (every? #(contains? (set b64-charset) %) (remove #{\=} s))
            "Invalid base64 character(s) found.")
    (assert (and (zero? (mod (count s) 4)) (< padding 3)) "Incorrect padding")
    (let [core (clojure.string/replace s #"=+$" "")
          bits (apply str (map #(let [b (format "%6s" (Integer/toBinaryString
                                                       (.indexOf ^String b64-charset (str %))))]
                                  (clojure.string/replace b #" " "0"))
                               core))
          bits (subs bits 0 (- (count bits) (* padding 2)))
          bytes (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 2))
                                 (partition 8 bits)))]
      (String. ^bytes bytes "UTF-8"))))
