(ns clojure-algorithms.blockchain.merkle-tree
  "Merkle tree construction and inclusion-proof verification for
  blockchain data integrity. Leaves are SHA-256 hashes of transactions;
  internal nodes hash the concatenation of their children.
  Ref: https://en.wikipedia.org/wiki/Merkle_tree")

(defn sha256
  "Returns the hexadecimal SHA-256 digest of string `data`."
  [data]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes ^String data "UTF-8"))
    (apply str (map #(format "%02x" (bit-and % 0xFF)) (.digest digest)))))

(defn build-merkle-tree
  "Builds a Merkle tree from `leaves` (data strings). Returns a vector of
  levels; the last level holds the single Merkle root. Throws on empty input."
  [leaves]
  (when (empty? leaves)
    (throw (ex-info "Leaf list cannot be empty." {})))
  (loop [current-level (mapv sha256 leaves)
         tree [(mapv sha256 leaves)]]
    (if (= 1 (count current-level))
      tree
      (let [next-level (mapv (fn [pair]
                               (let [left (first pair)
                                     right (if (= 2 (count pair)) (second pair) left)]
                                 (sha256 (str left right))))
                             (partition-all 2 current-level))]
        (recur next-level (conj tree next-level))))))

(defn merkle-root
  "Returns the Merkle root hash for `leaves`."
  [leaves]
  (peek (peek (build-merkle-tree leaves))))

(defn verify-proof
  "Returns true when `proof` (sibling hashes up the path) links `leaf`
  to `root`."
  [leaf proof root]
  (let [computed (reduce (fn [h sibling] (sha256 (str h sibling)))
                         (sha256 leaf)
                         proof)]
    (= computed root)))
