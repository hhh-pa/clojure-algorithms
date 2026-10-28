(ns clojure-algorithms.ciphers.rc4
  "RC4 stream cipher over byte vectors (educational use only).")

(defn ksa
  "Key scheduling: permutes the 256-byte state with key bytes."
  [key]
  (let [s (int-array (range 256))
        kl (count key)]
    (loop [i 0 j 0]
      (if (>= i 256)
        (vec s)
        (let [j (mod (+ j (aget s i) (nth key (mod i kl))) 256)
              t (aget s i)]
          (aset s i (aget s j))
          (aset s j t)
          (recur (inc i) j))))))

(defn prga
  "Lazy keystream bytes from a permuted state box."
  [s-box]
  (let [s (int-array s-box)]
    ((fn step [i j]
       (lazy-seq
        (let [i (mod (inc i) 256)
              j (mod (+ j (aget s i)) 256)
              t (aget s i)]
          (aset s i (aget s j))
          (aset s j t)
          (cons (aget s (mod (+ (aget s i) (aget s j)) 256))
                (step i j)))))
     0 0)))

(defn encrypt
  "XORs plain bytes with the RC4 keystream."
  [plaintext key]
  (when (empty? key)
    (throw (IllegalArgumentException. "Key must not be empty.")))
  (mapv bit-xor plaintext (prga (ksa (vec key)))))

(defn decrypt
  "Decrypts RC4 cipher bytes (same operation)."
  [ciphertext key]
  (encrypt ciphertext key))

(defn str-encrypt
  "Encrypts a UTF-8 string with a string key, returning cipher bytes."
  [plaintext key]
  (encrypt (mapv int (.getBytes ^String plaintext "UTF-8"))
           (mapv int (.getBytes ^String key "UTF-8"))))
