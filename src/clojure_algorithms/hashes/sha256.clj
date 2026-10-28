(ns clojure-algorithms.hashes.sha256
  "SHA-256 hash function. References:
  https://qvault.io/cryptography/how-sha-2-works-step-by-step-sha-256/
  https://en.wikipedia.org/wiki/SHA-2")

(def ^:private mask32 0xFFFFFFFF)

(defn- u32 [x] (bit-and (long x) mask32))

(defn- ror [value rotations]
  (u32 (bit-or (bit-shift-left (long value) (- 32 rotations))
               (unsigned-bit-shift-right (long value) rotations))))

(def ^:private round-constants
  [0x428A2F98 0x71374491 0xB5C0FBCF 0xE9B5DBA5 0x3956C25B 0x59F111F1 0x923F82A4 0xAB1C5ED5
   0xD807AA98 0x12835B01 0x243185BE 0x550C7DC3 0x72BE5D74 0x80DEB1FE 0x9BDC06A7 0xC19BF174
   0xE49B69C1 0xEFBE4786 0x0FC19DC6 0x240CA1CC 0x2DE92C6F 0x4A7484AA 0x5CB0A9DC 0x76F988DA
   0x983E5152 0xA831C66D 0xB00327C8 0xBF597FC7 0xC6E00BF3 0xD5A79147 0x06CA6351 0x14292967
   0x27B70A85 0x2E1B2138 0x4D2C6DFC 0x53380D13 0x650A7354 0x766A0ABB 0x81C2C92E 0x92722C85
   0xA2BFE8A1 0xA81A664B 0xC24B8B70 0xC76C51A3 0xD192E819 0xD6990624 0xF40E3585 0x106AA070
   0x19A4C116 0x1E376C08 0x2748774C 0x34B0BCB5 0x391C0CB3 0x4ED8AA4A 0x5B9CCA4F 0x682E6FF3
   0x748F82EE 0x78A5636F 0x84C87814 0x8CC70208 0x90BEFFFA 0xA4506CEB 0xBEF9A3F7 0xC67178F2])

(def ^:private initial-hashes
  [0x6A09E667 0xBB67AE85 0x3C6EF372 0xA54FF53A
   0x510E527F 0x9B05688C 0x1F83D9AB 0x5BE0CD19])

(defn- pad-message
  "Pads byte array per SHA-256: 0x80, zeros, then 64-bit big-endian bit length."
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

(defn sha256-bytes
  "Returns the SHA-256 digest of a byte array as a 64-char hex string."
  [^bytes message]
  (let [padded (pad-message message)
        nblocks (/ (alength padded) 64)]
    (loop [b 0 hashes initial-hashes]
      (if (= b nblocks)
        (apply str (map #(format "%08x" %) hashes))
        (let [w0 (mapv (fn [i]
                         (let [o (+ (* b 64) (* i 4))]
                           (u32 (+ (bit-shift-left (long (bit-and (aget padded o) 0xFF)) 24)
                                   (bit-shift-left (bit-and (aget padded (+ o 1)) 0xFF) 16)
                                   (bit-shift-left (bit-and (aget padded (+ o 2)) 0xFF) 8)
                                   (bit-and (aget padded (+ o 3)) 0xFF)))))
                       (range 16))
              words (reduce (fn [w i]
                              (let [s0 (u32 (bit-xor (ror (nth w (- i 15)) 7)
                                                     (ror (nth w (- i 15)) 18)
                                                     (unsigned-bit-shift-right (nth w (- i 15)) 3)))
                                    s1 (u32 (bit-xor (ror (nth w (- i 2)) 17)
                                                     (ror (nth w (- i 2)) 19)
                                                     (unsigned-bit-shift-right (nth w (- i 2)) 10)))]
                                (conj w (u32 (+ (nth w (- i 16)) s0 (nth w (- i 7)) s1)))))
                            w0 (range 16 64))
              [a b* c d e f g h]
              (loop [i 0 [a b c d e f g h] hashes]
                (if (= i 64)
                  [a b c d e f g h]
                  (let [s1 (u32 (bit-xor (ror e 6) (ror e 11) (ror e 25)))
                        ch (u32 (bit-xor (bit-and e f) (bit-and (u32 (bit-not e)) g)))
                        t1 (u32 (+ h s1 ch (nth round-constants i) (nth words i)))
                        s0 (u32 (bit-xor (ror a 2) (ror a 13) (ror a 22)))
                        maj (u32 (bit-xor (bit-and a b) (bit-and a c) (bit-and b c)))
                        t2 (u32 (+ s0 maj))]
                    (recur (inc i) [(u32 (+ t1 t2)) a b c (u32 (+ d t1)) e f g]))))]
          (recur (inc b) (mapv u32 (map + hashes [a b* c d e f g h]))))))))

(defn sha256
  "Returns the SHA-256 hex digest of string s (UTF-8)."
  [s]
  (sha256-bytes (.getBytes ^String s "UTF-8")))
