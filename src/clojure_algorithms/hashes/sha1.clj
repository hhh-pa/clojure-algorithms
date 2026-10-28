(ns clojure-algorithms.hashes.sha1
  "SHA-1 hash function (educational port; SHA-1 is broken and must not be
  used for security). Reference: https://deadhacker.com/2006/02/21/sha-1-illustrated/")

(def ^:private mask32 0xFFFFFFFF)

(defn- u32 [x] (bit-and (long x) mask32))

(defn- rol [n b]
  (u32 (bit-or (bit-shift-left (long n) b)
               (unsigned-bit-shift-right (long n) (- 32 b)))))

(defn- pad-message
  "Pads byte array per SHA-1: 0x80, zeros, then 64-bit big-endian bit length."
  [^bytes msg]
  (let [mlen (alength msg)
        bitlen (* 8 mlen)
        total (let [n (+ mlen 1 8)] (+ n (mod (- 64 (mod n 64)) 64)))
        out (byte-array total)]
    (System/arraycopy msg 0 out 0 mlen)
    (aset out mlen (unchecked-byte 0x80))
    (dotimes [i 8]
      (aset out (+ (- total 8) i)
            (unchecked-byte (bit-and (unsigned-bit-shift-right bitlen (* 8 (- 7 i))) 0xFF))))
    out))

(defn sha1-bytes
  "Returns the SHA-1 digest of a byte array as a 40-char hex string."
  [^bytes message]
  (let [padded (pad-message message)
        nblocks (/ (alength padded) 64)]
    (loop [b 0 h [0x67452301 0xEFCDAB89 0x98BADCFE 0x10325476 0xC3D2E1F0]]
      (if (= b nblocks)
        (apply str (map #(format "%08x" %) h))
        (let [w0 (mapv (fn [i]
                         (let [o (+ (* b 64) (* i 4))]
                           (u32 (+ (bit-shift-left (long (bit-and (aget padded o) 0xFF)) 24)
                                   (bit-shift-left (bit-and (aget padded (+ o 1)) 0xFF) 16)
                                   (bit-shift-left (bit-and (aget padded (+ o 2)) 0xFF) 8)
                                   (bit-and (aget padded (+ o 3)) 0xFF)))))
                       (range 16))
              w (reduce (fn [w i]
                          (conj w (rol (u32 (bit-xor (nth w (- i 3)) (nth w (- i 8))
                                                     (nth w (- i 14)) (nth w (- i 16)))) 1)))
                        w0 (range 16 80))
              [a b* c d e] (loop [i 0 a (nth h 0) b (nth h 1) c (nth h 2) d (nth h 3) e (nth h 4)]
                             (if (= i 80)
                               [a b c d e]
                               (let [[f k] (cond
                                             (< i 20) [(u32 (bit-or (bit-and b c) (bit-and (u32 (bit-not b)) d))) 0x5A827999]
                                             (< i 40) [(u32 (bit-xor b c d)) 0x6ED9EBA1]
                                             (< i 60) [(u32 (bit-or (bit-and b c) (bit-and b d) (bit-and c d))) 0x8F1BBCDC]
                                             :else [(u32 (bit-xor b c d)) 0xCA62C1D6])]
                                 (recur (inc i)
                                        (u32 (+ (rol a 5) f e k (nth w i)))
                                        a (rol b 30) c d))))]
          (recur (inc b) (mapv u32 (map + h [a b* c d e]))))))))

(defn sha1
  "Returns the SHA-1 hex digest of string s (UTF-8)."
  [s]
  (sha1-bytes (.getBytes ^String s "UTF-8")))
