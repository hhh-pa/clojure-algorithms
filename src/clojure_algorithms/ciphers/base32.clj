(ns clojure-algorithms.ciphers.base32
  "Base32 encoding over UTF-8 strings.")

(def b32-charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defn- str-to-bits
  [s]
  (apply str (map #(let [b (format "%8s" (Integer/toBinaryString (int %)))]
                     (clojure.string/replace b #" " "0"))
                  (.getBytes ^String s "UTF-8"))))

(defn base32-encode
  "Encodes a string to padded Base32."
  [data]
  (let [binary (str-to-bits data)
        padded-len (* 5 (inc (quot (count binary) 5)))
        padded (format (str "%-" padded-len "s") binary)
        padded (clojure.string/replace padded #" " "0")
        chunks (partition 5 padded)
        encoded (apply str (map #(nth b32-charset
                                      (Integer/parseInt (apply str %) 2))
                                chunks))
        out-len (* 8 (inc (quot (count encoded) 8)))]
    (apply str (take out-len (concat encoded (repeat \=))))))

(defn base32-decode
  "Decodes a padded Base32 string."
  [data]
  (let [stripped (clojure.string/replace data #"=+$" "")
        bits (apply str (map #(let [b (format "%5s" (Integer/toBinaryString
                                                     (.indexOf ^String b32-charset (str %))))]
                                (clojure.string/replace b #" " "0"))
                             stripped))
        bytes (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 2))
                               (partition 8 bits)))]
    (String. ^bytes bytes "UTF-8")))
