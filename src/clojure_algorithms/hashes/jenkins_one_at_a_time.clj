(ns clojure-algorithms.hashes.jenkins-one-at-a-time
  "Bob Jenkins' One-at-a-Time hash for general-purpose (non-crypto) use.
  See https://en.wikipedia.org/wiki/Jenkins_hash_function")

(def ^:private mask 0xFFFFFFFF)

(defn jenkins-one-at-a-time
  "Returns the Jenkins One-at-a-Time hash of string key."
  [key]
  (let [h (reduce (fn [h b]
                    (let [h (bit-and (+ h (bit-and b 0xFF)) mask)
                          h (bit-and (+ h (bit-shift-left h 10)) mask)]
                      (bit-and (bit-xor h (unsigned-bit-shift-right h 6)) mask)))
                  0
                  (.getBytes ^String key "UTF-8"))
        h (bit-and (+ h (bit-shift-left h 3)) mask)
        h (bit-and (bit-xor h (unsigned-bit-shift-right h 11)) mask)
        h (bit-and (+ h (bit-shift-left h 15)) mask)]
    h))
