(ns clojure-algorithms.ciphers.base16
  "Base16 (hex) encoding over UTF-8 strings.")

(defn base16-encode
  "Encodes a string to uppercase hex."
  [data]
  (apply str (map #(format "%02X" (int %)) (.getBytes ^String data "UTF-8"))))

(defn base16-decode
  "Decodes an uppercase hex string back to a string."
  [data]
  (when (odd? (count data))
    (throw (IllegalArgumentException.
            (str "Base16 encoded data is invalid:\n"
                 "Data does not have an even number of hex digits."))))
  (when-not (every? #(contains? (set "0123456789ABCDEF") %) data)
    (throw (IllegalArgumentException.
            (str "Base16 encoded data is invalid:\n"
                 "Data is not uppercase hex or it contains invalid characters."))))
  (let [bytes (byte-array (map #(unchecked-byte
                                 (Integer/parseInt (subs data % (+ % 2)) 16))
                               (range 0 (count data) 2)))]
    (String. ^bytes bytes "UTF-8")))
