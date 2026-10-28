(ns clojure-algorithms.blockchain.proof-of-work
  "Proof-of-Work consensus: miners adjust a block's nonce until its
  SHA-256 hash carries `difficulty` leading zeros.")

(defn- sha256-hex
  [s]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes ^String s "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xFF)) (.digest digest)))))

(defrecord Block [index previous-hash transactions timestamp nonce difficulty hash])

(defn compute-hash
  "Returns the SHA-256 hash of the block's content fields."
  [block]
  (sha256-hex (str (:index block) (:previous-hash block) (:transactions block)
                   (:timestamp block) (:nonce block))))

(defn make-block
  "Creates an unmined block; the initial hash uses nonce 0."
  [index previous-hash transactions timestamp difficulty]
  (let [block (->Block index previous-hash transactions timestamp 0 difficulty nil)]
    (assoc block :hash (compute-hash block))))

(defn mine-block
  "Returns `block` with nonce/hash adjusted until the hash has `difficulty`
  leading zeros."
  [block]
  (let [target (apply str (repeat (:difficulty block) "0"))]
    (loop [b block]
      (if (= target (subs (:hash b) 0 (:difficulty b)))
        b
        (let [next (update b :nonce inc)]
          (recur (assoc next :hash (compute-hash next))))))))

(defrecord Blockchain [chain difficulty])

(defn make-blockchain
  "Creates a chain holding a single mined genesis block."
  [difficulty]
  (->Blockchain [(mine-block (make-block 0 "0" "Genesis Block"
                                         (double (System/currentTimeMillis))
                                         difficulty))]
                difficulty))

(defn add-block
  "Mines a block carrying `transactions` and appends it to `chain`."
  [chain transactions]
  (let [prev (peek (:chain chain))
        block (mine-block (make-block (count (:chain chain)) (:hash prev)
                                      transactions
                                      (double (System/currentTimeMillis))
                                      (:difficulty chain)))]
    (update chain :chain conj block)))

(defn chain-valid?
  "Returns true when every block's stored hash matches its content and
  every previous-hash link matches."
  [chain]
  (let [blocks (:chain chain)]
    (every? true?
            (map-indexed (fn [i block]
                           (if (zero? i)
                             true
                             (and (= (:hash block) (compute-hash block))
                                  (= (:previous-hash block)
                                     (:hash (nth blocks (dec i)))))))
                         blocks))))
