(ns clojure-algorithms.other.sdes
  "Simplified DES (S-DES) encryption and decryption.

  Port of TheAlgorithms/Python `other/sdes.py`.
  Bit strings are plain strings of \\0/\\1; the stdin-driven demo from
  `__main__` is replaced by pure `encrypt`/`decrypt` functions.")

(def p8-table [6 3 7 4 8 5 10 9])
(def p10-table [3 5 2 7 4 10 1 9 8 6])
(def p4-table [2 4 3 1])
(def ip [2 6 3 1 4 8 5 7])
(def ip-inv [4 1 3 5 7 2 8 6])
(def expansion [4 1 2 3 2 3 4 1])
(def s0 [[1 0 3 2] [3 2 1 0] [0 2 1 3] [3 1 3 2]])
(def s1 [[0 1 2 3] [2 0 1 3] [3 0 1 0] [2 1 0 3]])

(defn apply-table
  "Permute bit string `inp` by 1-based index `table`.
  Index 0 wraps to the last character, matching the Python original's
  negative-index doctest."
  [inp table]
  (let [n (count inp)]
    (apply str (map #(nth inp (mod (dec %) n)) table))))

(defn left-shift
  "Rotate bit string `data` one position left."
  [data]
  (str (subs data 1) (subs data 0 1)))

(defn xor-bits
  "Bitwise XOR of equal-length bit strings `a` and `b`."
  [a b]
  (apply str (map #(if (= %1 %2) \0 \1) a b)))

(defn- pad2
  [s]
  (str (apply str (repeat (- 2 (count s)) \0)) s))

(defn apply-sbox
  "Look up 4-bit `data` in S-box `s`; return a 2-bit string."
  [s data]
  (let [row (Integer/parseInt (str (first data) (last data)) 2)
        col (Integer/parseInt (subs data 1 3) 2)]
    (pad2 (Integer/toBinaryString (get-in s [row col])))))

(defn subkeys
  "Derive the two 8-bit round keys from 10-bit `key`."
  [key]
  (let [temp (apply-table key p10-table)
        left (left-shift (subs temp 0 5))
        right (left-shift (subs temp 5))
        key1 (apply-table (str left right) p8-table)
        left3 (nth (iterate left-shift left) 3)
        right3 (nth (iterate left-shift right) 3)
        key2 (apply-table (str left3 right3) p8-table)]
    [key1 key2]))

(defn- feistel
  [key message]
  (let [left (subs message 0 4)
        right (subs message 4)
        temp (xor-bits (apply-table right expansion) key)
        temp (apply-table (str (apply-sbox s0 (subs temp 0 4))
                               (apply-sbox s1 (subs temp 4)))
                          p4-table)]
    (str (xor-bits left temp) right)))

(defn encrypt
  "Encrypt 8-bit `message` with 10-bit `key`; return 8-bit ciphertext."
  [key message]
  (let [[key1 key2] (subkeys key)
        temp (apply-table message ip)
        temp (feistel key1 temp)
        temp (str (subs temp 4) (subs temp 0 4))
        temp (feistel key2 temp)]
    (apply-table temp ip-inv)))

(defn decrypt
  "Decrypt 8-bit `ciphertext` with 10-bit `key`."
  [key ciphertext]
  (let [[key1 key2] (subkeys key)
        temp (apply-table ciphertext ip)
        temp (feistel key2 temp)
        temp (str (subs temp 4) (subs temp 0 4))
        temp (feistel key1 temp)]
    (apply-table temp ip-inv)))
