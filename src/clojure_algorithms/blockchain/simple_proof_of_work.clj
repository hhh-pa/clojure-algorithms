(ns clojure-algorithms.blockchain.simple-proof-of-work
  "Proof-of-Work: find a nonce so the SHA-256 hash of the block data
  starts with `difficulty` zeros.")

(defn- sha256-hex
  [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes ^String s "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xFF)) (.digest digest)))))

(defn proof-of-work
  "Returns [nonce hash]: the first nonce whose block hash meets `difficulty`.
  Difficulty must be non-negative; 0 returns nonce 0 immediately."
  [block-number transactions previous-hash difficulty]
  (when (neg? difficulty)
    (throw (ex-info "difficulty must be a non-negative integer" {})))
  (let [prefix (apply str (repeat difficulty "0"))]
    (loop [nonce 0]
      (let [h (sha256-hex (str block-number transactions previous-hash nonce))]
        (if (.startsWith ^String h prefix)
          [nonce h]
          (recur (inc nonce)))))))
