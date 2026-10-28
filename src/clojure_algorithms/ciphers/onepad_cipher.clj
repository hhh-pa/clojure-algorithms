(ns clojure-algorithms.ciphers.onepad-cipher
  "One-pad cipher with pseudo-random keys.")

(defn encrypt
  "Encrypts text, returning [cipher key] integer vectors."
  ([text] (encrypt text (java.util.Random.)))
  ([text rng]
   (let [pairs (mapv (fn [ch]
                       (let [k (inc (.nextInt ^java.util.Random rng 300))]
                         [(* (+ (int ch) k) k) k]))
                     text)]
     [(mapv first pairs) (mapv second pairs)])))

(defn decrypt
  "Decrypts cipher integers with key integers back to text."
  [cipher key]
  (apply str (map (fn [c k] (char (int (/ (- c (* k k)) k)))) cipher key)))
