(ns clojure-algorithms.hashes.crc32
  "CRC32 (IEEE 802.3 polynomial 0xEDB88320) for error detection.
  Not for cryptographic use. See https://en.wikipedia.org/wiki/Cyclic_redundancy_check")

(def ^:private polynomial 0xEDB88320)

(defn- generate-table
  "Builds the 256-entry CRC32 lookup table."
  []
  (mapv (fn [i]
          (loop [crc i n 8]
            (if (zero? n)
              crc
              (recur (if (odd? crc)
                       (bit-xor (unsigned-bit-shift-right crc 1) polynomial)
                       (unsigned-bit-shift-right crc 1))
                     (dec n)))))
        (range 256)))

(def crc32-table (generate-table))

(defn crc32
  "Returns the CRC32 hash of byte-data (a byte array or seq of bytes)
  as an unsigned 32-bit integer."
  [byte-data]
  (let [crc (reduce (fn [crc b]
                      (bit-xor (unsigned-bit-shift-right crc 8)
                               (nth crc32-table (bit-and (bit-xor crc (bit-and b 0xFF)) 0xFF))))
                    0xFFFFFFFF
                    (seq byte-data))]
    (bit-xor crc 0xFFFFFFFF)))

(defn crc32-str
  "Returns the CRC32 hash of the UTF-8 bytes of string s."
  [s]
  (crc32 (.getBytes ^String s "UTF-8")))
