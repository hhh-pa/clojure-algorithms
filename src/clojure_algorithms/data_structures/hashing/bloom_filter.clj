(ns clojure-algorithms.data-structures.hashing.bloom-filter
  "Port of data_structures/hashing/bloom_filter.py.
  Bloom filter with SHA-256 and MD5 positions.
  State is {:size n :bitarray i}.")

(defn make-bloom
  "Bloom filter with SIZE bits."
  ([] (make-bloom 8))
  ([size] {:size size :bitarray 0}))

(defn- digest-little [algorithm value]
  (let [md (java.security.MessageDigest/getInstance algorithm)
        bytes (.digest md (.getBytes ^String value "UTF-8"))]
    (BigInteger. 1 (byte-array (reverse (seq bytes))))))

(defn bloom-hash
  "Bit mask for VALUE (one bit per hash function)."
  [{:keys [size]} value]
  (reduce (fn [res algo]
            (let [pos (.intValue (.mod (digest-little algo value)
                                       (BigInteger/valueOf size)))]
              (bit-or res (bit-shift-left 1 pos))))
          0 ["SHA-256" "MD5"]))

(defn format-bin
  "Binary string of BITARRAY left-padded to the filter size."
  [{:keys [size]} bitarray]
  (let [s (Long/toBinaryString (long bitarray))]
    (str (apply str (repeat (- size (count s)) \0)) s)))

(defn bitstring
  "Current bit array as a binary string."
  [{:keys [bitarray] :as bloom}]
  (format-bin bloom bitarray))

(defn format-hash
  "Bit mask for VALUE as a binary string."
  [bloom value]
  (format-bin bloom (bloom-hash bloom value)))

(defn bloom-add
  "Add VALUE; returns the new filter."
  [bloom value]
  (update bloom :bitarray bit-or (bloom-hash bloom value)))

(defn bloom-exists?
  "True when VALUE is (probably) present."
  [{:keys [bitarray] :as bloom} value]
  (let [h (bloom-hash bloom value)]
    (= (bit-and h bitarray) h)))

(defn estimated-error-rate
  "Estimated false-positive rate from bit density."
  [{:keys [size bitarray]}]
  (Math/pow (/ (double (Long/bitCount (long bitarray))) size) 2))
