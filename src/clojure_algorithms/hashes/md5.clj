(ns clojure-algorithms.hashes.md5
  "MD5 message-digest algorithm (educational port; MD5 is broken and must
  not be used for security). See https://en.wikipedia.org/wiki/MD5")

(def ^:private mask32 0xFFFFFFFF)

(defn- u32 [x] (bit-and (long x) mask32))

(defn- rol32 [x s]
  (let [x (long x)]
    (u32 (bit-or (bit-shift-left x s)
                 (unsigned-bit-shift-right x (- 32 s))))))

(def ^:private shifts
  [7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
   5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
   4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
   6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21])

(def ^:private table-t
  (mapv (fn [i] (u32 (long (* 4294967296 (Math/abs (Math/sin (inc i)))))))
        (range 64)))

(defn- pad-message
  "Pads msg per MD5: 0x80, zeros, then 64-bit little-endian bit length."
  [^bytes msg]
  (let [mlen (alength msg)
        bitlen (* 8 mlen)
        n (+ mlen 1 8)
        total (+ n (mod (- 64 (mod n 64)) 64))
        out (byte-array total)]
    (System/arraycopy msg 0 out 0 mlen)
    (aset out mlen (unchecked-byte 0x80))
    (dotimes [i 8]
      (let [shift (* 8 i)
            b (bit-and (unsigned-bit-shift-right bitlen shift) 0xFF)]
        (aset out (+ (- total 8) i) (unchecked-byte b))))
    out))

(defn- block-words
  "Reads 16 little-endian 32-bit words for block b from padded."
  [^bytes padded b]
  (mapv (fn [i]
          (let [o (+ (* b 64) (* i 4))
                b0 (bit-and (aget padded o) 0xFF)
                b1 (bit-and (aget padded (+ o 1)) 0xFF)
                b2 (bit-and (aget padded (+ o 2)) 0xFF)
                b3 (bit-and (aget padded (+ o 3)) 0xFF)]
            (u32 (+ b0
                    (bit-shift-left b1 8)
                    (bit-shift-left b2 16)
                    (bit-shift-left (long b3) 24)))))
        (range 16)))

(defn- round-fn
  "Returns [f g] for round i given state a b c d."
  [i a b c d]
  (cond
    (< i 16) [(u32 (bit-xor d (bit-and b (bit-xor c d)))) i]
    (< i 32) [(u32 (bit-xor c (bit-and d (bit-xor b c)))) (mod (+ (* 5 i) 1) 16)]
    (< i 48) [(u32 (bit-xor (bit-xor b c) d)) (mod (+ (* 3 i) 5) 16)]
    :else [(u32 (bit-xor c (bit-or b (u32 (bit-not d))))) (mod (* 7 i) 16)]))

(defn- compress-block
  "Runs 64 MD5 rounds over words with state [a0 b0 c0 d0].
  Returns the new [a b c d]."
  [words a0 b0 c0 d0]
  (loop [i 0 a a0 b b0 c c0 d d0]
    (if (= i 64)
      [a b c d]
      (let [[f g] (round-fn i a b c d)
            f (u32 (+ f a (nth table-t i) (nth words g)))
            new-b (u32 (+ b (rol32 f (nth shifts i))))]
        (recur (inc i) d new-b b c)))))

(defn- word-hex
  "Formats a 32-bit word as 8 little-endian hex chars."
  [w]
  (apply str (map (fn [i]
                    (format "%02x" (bit-and (unsigned-bit-shift-right (long w) (* 8 i)) 0xFF)))
                  (range 4))))

(defn md5-bytes
  "Returns the MD5 digest of a byte array as a 32-char hex string."
  [^bytes message]
  (let [padded (pad-message message)
        nblocks (quot (alength padded) 64)]
    (loop [b 0
           a0 0x67452301
           b0 0xEFCDAB89
           c0 0x98BADCFE
           d0 0x10325476]
      (if (= b nblocks)
        (apply str (map word-hex [a0 b0 c0 d0]))
        (let [words (block-words padded b)
              [a b* c d] (compress-block words a0 b0 c0 d0)]
          (recur (inc b)
                 (u32 (+ a0 a))
                 (u32 (+ b0 b*))
                 (u32 (+ c0 c))
                 (u32 (+ d0 d))))))))

(defn md5
  "Returns the MD5 hex digest of string s (UTF-8)."
  [s]
  (md5-bytes (.getBytes ^String s "UTF-8")))
