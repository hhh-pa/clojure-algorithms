(ns clojure-algorithms.conversions.endianness
  "Port of conversions/endianness.py.
  Byte-order (endianness) conversion utilities.
  Byte sequences are represented as vectors of byte-value integers (0-255).")

(defn swap-endianness-16
  "Swap the byte order of a 16-bit integer."
  [value]
  (when-not (<= 0 value 0xFFFF)
    (throw (ex-info (str "value must be between 0 and " 0xFFFF)
                    {:type :value-error})))
  (bit-or (bit-shift-left (bit-and value 0xFF) 8)
          (bit-shift-right (bit-and value 0xFF00) 8)))

(defn swap-endianness-32
  "Swap the byte order of a 32-bit integer."
  [value]
  (when-not (<= 0 value 0xFFFFFFFF)
    (throw (ex-info (str "value must be between 0 and " 0xFFFFFFFF)
                    {:type :value-error})))
  (bit-or (bit-shift-left (bit-and value 0x000000FF) 24)
          (bit-shift-left (bit-and value 0x0000FF00) 8)
          (bit-shift-right (bit-and value 0x00FF0000) 8)
          (unsigned-bit-shift-right (bit-and value 0xFF000000) 24)))

(defn swap-endianness-64
  "Swap the byte order of a 64-bit integer."
  [value]
  (when-not (<= 0 value 0xFFFFFFFFFFFFFFFF)
    (throw (ex-info (str "value must be between 0 and " 0xFFFFFFFFFFFFFFFF)
                    {:type :value-error})))
  (let [v (bigint value)
        pows (take 8 (iterate (partial * 256N) 1N))
        bytes (map (fn [p] (mod (quot v p) 256)) pows)]
    (reduce + (map * bytes (reverse pows)))))

(defn- check-data [data]
  (when-not (and (sequential? data) (<= 1 (count data) 8))
    (throw (ex-info "data must be between 1 and 8 bytes"
                    {:type :value-error}))))

(defn bytes-to-int-little
  "Convert byte vector DATA to an integer (little-endian)."
  [data]
  (check-data data)
  (reduce (fn [acc [i b]] (+ acc (bit-shift-left (long b) (* 8 i))))
          0 (map-indexed vector data)))

(defn bytes-to-int-big
  "Convert byte vector DATA to an integer (big-endian)."
  [data]
  (check-data data)
  (reduce (fn [acc b] (+ (bit-shift-left acc 8) (long b))) 0 data))

(defn- int-to-bytes [value num-bytes little?]
  (when (neg? value)
    (throw (ex-info "value must be non-negative" {:type :value-error})))
  (when-not (<= 1 num-bytes 8)
    (throw (ex-info "num_bytes must be between 1 and 8" {:type :value-error})))
  (when (>= value (bit-shift-left 1 (* num-bytes 8)))
    (throw (ex-info (str "value " value " too large for " num-bytes " bytes")
                    {:type :value-error})))
  (let [bs (mapv (fn [i] (bit-and (bit-shift-right value (* 8 i)) 0xFF))
                 (range num-bytes))]
    (if little? bs (vec (reverse bs)))))

(defn int-to-bytes-little
  "Convert integer VALUE to a NUM-BYTES byte vector (little-endian)."
  [value num-bytes]
  (int-to-bytes value num-bytes true))

(defn int-to-bytes-big
  "Convert integer VALUE to a NUM-BYTES byte vector (big-endian)."
  [value num-bytes]
  (int-to-bytes value num-bytes false))
