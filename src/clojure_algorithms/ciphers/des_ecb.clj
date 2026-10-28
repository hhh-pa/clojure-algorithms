(ns clojure-algorithms.ciphers.des-ecb
  "DES block cipher in ECB mode over 0/1 bit vectors.")

(def ip [58 50 42 34 26 18 10 2 60 52 44 36 28 20 12 4 62 54 46 38 30 22 14 6
         64 56 48 40 32 24 16 8 57 49 41 33 25 17 9 1 59 51 43 35 27 19 11 3
         61 53 45 37 29 21 13 5 63 55 47 39 31 23 15 7])
(def ip-inv [40 8 48 16 56 24 64 32 39 7 47 15 55 23 63 31 38 6 46 14 54 22 62 30
             37 5 45 13 53 21 61 29 36 4 44 12 52 20 60 28 35 3 43 11 51 19 59 27
             34 2 42 10 50 18 58 26 33 1 41 9 49 17 57 25])
(def expansion [32 1 2 3 4 5 4 5 6 7 8 9 8 9 10 11 12 13 12 13 14 15 16 17
                16 17 18 19 20 21 20 21 22 23 24 25 24 25 26 27 28 29 28 29 30 31 32 1])
(def perm-p [16 7 20 21 29 12 28 17 1 15 23 26 5 18 31 10 2 8 24 14 32 27 3 9
             19 13 30 6 22 11 4 25])
(def s-boxes
  [[[14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7]
    [0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8]
    [4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0]
    [15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13]]
   [[15 1 8 14 6 11 3 4 9 7 2 13 12 0 5 10]
    [3 13 4 7 15 2 8 14 12 0 1 10 6 9 11 5]
    [0 14 7 11 10 4 13 1 5 8 12 6 9 3 2 15]
    [13 8 10 1 3 15 4 2 11 6 7 12 0 5 14 9]]
   [[10 0 9 14 6 3 15 5 1 13 12 7 11 4 2 8]
    [13 7 0 9 3 4 6 10 2 8 5 14 12 11 15 1]
    [13 6 4 9 8 15 3 0 11 1 2 12 5 10 14 7]
    [1 10 13 0 6 9 8 7 4 15 14 3 11 5 2 12]]
   [[7 13 14 3 0 6 9 10 1 2 8 5 11 12 4 15]
    [13 8 11 5 6 15 0 3 4 7 2 12 1 10 14 9]
    [10 6 9 0 12 11 7 13 15 1 3 14 5 2 8 4]
    [3 15 0 6 10 1 13 8 9 4 5 11 12 7 2 14]]
   [[2 12 4 1 7 10 11 6 8 5 3 15 13 0 14 9]
    [14 11 2 12 4 7 13 1 5 0 15 10 3 9 8 6]
    [4 2 1 11 10 13 7 8 15 9 12 5 6 3 0 14]
    [11 8 12 7 1 14 2 13 6 15 0 9 10 4 5 3]]
   [[12 1 10 15 9 2 6 8 0 13 3 4 14 7 5 11]
    [10 15 4 2 7 12 9 5 6 1 13 14 0 11 3 8]
    [9 14 15 5 2 8 12 3 7 0 4 10 1 13 11 6]
    [4 3 2 12 9 5 15 10 11 14 1 7 6 0 8 13]]
   [[4 11 2 14 15 0 8 13 3 12 9 7 5 10 6 1]
    [13 0 11 7 4 9 1 10 14 3 5 12 2 15 8 6]
    [1 4 11 13 12 3 7 14 10 15 6 8 0 5 9 2]
    [6 11 13 8 1 4 10 7 9 5 0 15 14 2 3 12]]
   [[13 2 8 4 6 15 11 1 10 9 3 14 5 0 12 7]
    [1 15 13 8 10 3 7 4 12 5 6 11 0 14 9 2]
    [7 11 4 1 9 12 14 2 0 6 10 13 15 3 5 8]
    [2 1 14 7 4 10 8 13 15 12 9 0 3 5 6 11]]])
(def pc1 [57 49 41 33 25 17 9 1 58 50 42 34 26 18 10 2 59 51 43 35 27 19 11 3
          60 52 44 36 63 55 47 39 31 23 15 7 62 54 46 38 30 22 14 6 61 53 45 37 29
          21 13 5 28 20 12 4])
(def pc2 [14 17 11 24 1 5 3 28 15 6 21 10 23 19 12 4 26 8 16 7 27 20 13 2
          41 52 31 37 47 55 30 40 51 45 33 48 44 49 39 56 34 53 46 42 50 36 29 32])

(defn string-to-bitset
  "Converts a string to a vector of 0/1 bits."
  [s]
  (vec (mapcat (fn [ch]
                 (let [v (int ch)]
                   (mapv #(bit-and (bit-shift-right v %) 1) (range 7 -1 -1))))
               s)))

(defn pad-right-to-multiple-of-n
  "Pads bitset with trailing zeros to a multiple of n."
  [bitset n]
  (let [r (mod (count bitset) n)]
    (if (zero? r) (vec bitset)
        (vec (concat bitset (repeat (- n r) 0))))))

(defn pad-left-to-multiple-of-n
  "Pads bitset with leading zeros to a multiple of n."
  [bitset n]
  (let [r (mod (count bitset) n)]
    (if (zero? r) (vec bitset)
        (vec (concat (repeat (- n r) 0) bitset)))))

(defn bitset-to-hex
  "Converts a bitset to a hex string of at least 16 digits."
  [bitset]
  (let [h (.toString (BigInteger. (apply str bitset) 2) 16)]
    (str (apply str (repeat (max 0 (- 16 (count h))) \0)) h)))

(defn hex-to-bitset
  "Converts a hex string to a bitset padded left to left-pad bits."
  [hex-string left-pad]
  (let [s (.toString (BigInteger. ^String hex-string 16) 2)
        s (str (apply str (repeat (max 0 (- left-pad (count s))) \0)) s)]
    (pad-left-to-multiple-of-n (mapv #(Character/digit ^char % 10) s) left-pad)))

(defn bitset-to-string
  "Interprets every 8 bits as a character."
  [bitset]
  (apply str (map (fn [byte-bits]
                    (char (reduce (fn [acc b] (+ (* acc 2) b)) 0 byte-bits)))
                  (partition 8 bitset))))

(defn xor-bits
  "Bitwise XOR of two equal-length bitsets."
  [b1 b2]
  (when (not= (count b1) (count b2))
    (throw (IllegalArgumentException. "Bitsets must be of the same length")))
  (mapv bit-xor b1 b2))

(defn shift-left
  "Rotates a bitset left by position places."
  [bitset position]
  (let [p (mod position (count bitset))]
    (vec (concat (subvec (vec bitset) p) (subvec (vec bitset) 0 p)))))

(defn permute-bits
  "Reorders a bitset by a 1-based permutation table."
  [bitset permutation]
  (when-not (every? #(and (pos? %) (<= % (count bitset))) permutation)
    (throw (IllegalArgumentException.
            "Permutation values must be within the range of the bitset")))
  (mapv #(nth bitset (dec %)) permutation))

(defn generate-key
  "Generates a random 16-character hex key."
  []
  (apply str (repeatedly 16 #(rand-nth "0123456789abcdef"))))

(defn key-schedule
  "Derives 16 48-bit round keys from a 64-bit hex key."
  [key]
  (let [permuted (permute-bits (hex-to-bitset key 64) pc1)]
    (loop [left (subvec permuted 0 28) right (subvec permuted 28 56)
           round 1 keys []]
      (if (> round 16)
        keys
        (let [sh (if (#{1 2 9 16} round) 1 2)
              left (shift-left left sh)
              right (shift-left right sh)]
          (recur left right (inc round)
                 (conj keys (permute-bits (vec (concat left right)) pc2))))))))

(defn des
  "Encrypts a 64-bit-aligned bitset with 16 round keys."
  [keys plain-bitset]
  (let [blocks (quot (count plain-bitset) 64)]
    (vec (mapcat
          (fn [b]
            (let [block (subvec (vec plain-bitset) (* b 64) (* (inc b) 64))
                  swapped (permute-bits block ip)
                  [left right] [(subvec swapped 0 32) (subvec swapped 32 64)]
                  [left right]
                  (loop [rnd 0 left left right right]
                    (if (>= rnd 16)
                      [left right]
                      (let [expanded (permute-bits right expansion)
                            xored (xor-bits expanded (nth keys rnd))
                            s-out (vec (mapcat
                                        (fn [j]
                                          (let [in (subvec xored (* j 6) (* (inc j) 6))
                                                row (+ (* 2 (nth in 0)) (nth in 5))
                                                col (+ (* 8 (nth in 1)) (* 4 (nth in 2))
                                                       (* 2 (nth in 3)) (nth in 4))
                                                v (get-in s-boxes [j row col])]
                                            (mapv #(bit-and (bit-shift-right v %) 1)
                                                  [3 2 1 0])))
                                        (range 8)))
                            p-out (permute-bits s-out perm-p)]
                        (recur (inc rnd) right (xor-bits left p-out)))))]
              (permute-bits (vec (concat right left)) ip-inv)))
          (range blocks)))))

(defn encrypt
  "Encrypts a string with a hex key, returning hex ciphertext."
  [key input-string]
  (->> (string-to-bitset input-string)
       (#(pad-right-to-multiple-of-n % 64))
       (des (key-schedule key))
       (bitset-to-hex)))

(defn decrypt
  "Decrypts hex ciphertext with a hex key back to a string."
  [key cipher-text]
  (->> (hex-to-bitset cipher-text 64)
       (des (vec (reverse (key-schedule key))))
       (bitset-to-string)
       (#(clojure.string/replace % #"\x00" ""))))
