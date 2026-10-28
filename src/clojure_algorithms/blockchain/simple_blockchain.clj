(ns clojure-algorithms.blockchain.simple-blockchain
  "A minimal blockchain with Proof-of-Work mining and integrity checks.
  Educational port of the Python simple_blockchain example.")

(defn- sha256-hex
  [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes ^String s "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xFF)) (.digest digest)))))

(defrecord Block [index timestamp data previous-hash nonce hash])

(defn compute-hash
  "Returns the SHA-256 hash of `block` combined with `nonce`."
  [block nonce]
  (sha256-hex (str (:index block) (:timestamp block) (:data block)
                   (:previous-hash block) nonce)))

(defn mine-block
  "Returns [nonce hash]: the first nonce whose hash carries `difficulty`
  leading zeros. Difficulty must be >= 1."
  [block difficulty]
  (when (< difficulty 1)
    (throw (ex-info "Difficulty must be at least 1" {})))
  (let [target (apply str (repeat difficulty "0"))]
    (loop [nonce 0]
      (let [h (compute-hash block nonce)]
        (if (.startsWith ^String h target)
          [nonce h]
          (recur (inc nonce)))))))

(defn make-block
  "Creates and mines a block with `index`, `data` and `previous-hash`."
  [index data previous-hash difficulty]
  (let [block (->Block index (double (System/currentTimeMillis)) data
                       previous-hash 0 nil)
        [nonce h] (mine-block block difficulty)]
    (assoc block :nonce nonce :hash h)))

(defrecord Blockchain [chain difficulty])

(defn create-genesis-block
  "Returns the mined genesis block for `difficulty`."
  [difficulty]
  (make-block 0 "Genesis Block" "0" difficulty))

(defn make-blockchain
  "Creates a chain holding just the genesis block."
  ([] (make-blockchain 2))
  ([difficulty] (->Blockchain [(create-genesis-block difficulty)] difficulty)))

(defn add-block
  "Mines a block carrying `data` and appends it; returns [chain block]."
  [chain data]
  (let [block (make-block (count (:chain chain)) data
                          (:hash (peek (:chain chain)))
                          (:difficulty chain))]
    [(update chain :chain conj block) block]))

(defn valid?
  "Returns true when every link, difficulty prefix and stored hash checks out."
  [chain]
  (let [blocks (:chain chain)
        target (apply str (repeat (:difficulty chain) "0"))]
    (every? true?
            (map-indexed
             (fn [i block]
               (if (zero? i)
                 true
                 (and (= (:previous-hash block) (:hash (nth blocks (dec i))))
                      (.startsWith ^String (:hash block) target)
                      (= (:hash block) (compute-hash block (:nonce block))))))
             blocks))))
