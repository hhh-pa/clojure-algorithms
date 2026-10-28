(ns clojure-algorithms.ciphers.xtea
  "XTEA block cipher over 8-byte blocks and 16-byte keys.")

(def delta 0x9E3779B9)
(def ^:private mask 0xFFFFFFFF)

(defn- u32 [v] (bit-and (long v) mask))

(defn- bytes-to-u32
  [bs]
  (u32 (+ (bit-shift-left (long (nth bs 0)) 24)
          (bit-shift-left (long (nth bs 1)) 16)
          (bit-shift-left (long (nth bs 2)) 8)
          (long (nth bs 3)))))

(defn- u32-to-bytes
  [v]
  [(bit-and (bit-shift-right v 24) 0xff) (bit-and (bit-shift-right v 16) 0xff)
   (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)])

(defn xtea-encrypt
  "Encrypts one 8-byte block with a 16-byte key."
  ([block key] (xtea-encrypt block key 64))
  ([block key num-rounds]
   (when-not (= 8 (count block))
     (throw (IllegalArgumentException. "block must be 8 bytes")))
   (when-not (= 16 (count key))
     (throw (IllegalArgumentException. "key must be 16 bytes")))
   (let [k (mapv #(bytes-to-u32 (subvec (vec key) % (+ % 4))) [0 4 8 12])]
     (loop [v0 (bytes-to-u32 (subvec (vec block) 0 4))
            v1 (bytes-to-u32 (subvec (vec block) 4 8))
            total 0
            round 0]
       (if (>= round num-rounds)
         (vec (concat (u32-to-bytes v0) (u32-to-bytes v1)))
         (let [v0 (u32 (+ v0 (bit-xor (u32 (+ (u32 (bit-xor (bit-shift-left v1 4)
                                                            (bit-shift-right v1 5)))
                                              v1))
                                      (u32 (+ total (nth k (bit-and total 3)))))))
               total (u32 (+ total delta))
               v1 (u32 (+ v1 (bit-xor (u32 (+ (u32 (bit-xor (bit-shift-left v0 4)
                                                            (bit-shift-right v0 5)))
                                              v0))
                                      (u32 (+ total (nth k (bit-and (bit-shift-right total 11) 3)))))))]
           (recur v0 v1 total (inc round))))))))

(defn xtea-decrypt
  "Decrypts one 8-byte block with a 16-byte key."
  ([block key] (xtea-decrypt block key 64))
  ([block key num-rounds]
   (when-not (= 8 (count block))
     (throw (IllegalArgumentException. "block must be 8 bytes")))
   (when-not (= 16 (count key))
     (throw (IllegalArgumentException. "key must be 16 bytes")))
   (let [k (mapv #(bytes-to-u32 (subvec (vec key) % (+ % 4))) [0 4 8 12])]
     (loop [v0 (bytes-to-u32 (subvec (vec block) 0 4))
            v1 (bytes-to-u32 (subvec (vec block) 4 8))
            total (u32 (* delta num-rounds))
            round 0]
       (if (>= round num-rounds)
         (vec (concat (u32-to-bytes v0) (u32-to-bytes v1)))
         (let [v1 (u32 (- v1 (bit-xor (u32 (+ (u32 (bit-xor (bit-shift-left v0 4)
                                                            (bit-shift-right v0 5)))
                                              v0))
                                      (u32 (+ total (nth k (bit-and (bit-shift-right total 11) 3)))))))
               total (u32 (- total delta))
               v0 (u32 (- v0 (bit-xor (u32 (+ (u32 (bit-xor (bit-shift-left v1 4)
                                                            (bit-shift-right v1 5)))
                                              v1))
                                      (u32 (+ total (nth k (bit-and total 3)))))))]
           (recur v0 v1 total (inc round))))))))
