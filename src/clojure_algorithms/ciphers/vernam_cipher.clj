(ns clojure-algorithms.ciphers.vernam-cipher
  "Vernam (one-time-pad style) cipher over A-Z.")

(defn vernam-encrypt
  "Encrypts uppercase plaintext with an uppercase key."
  [plaintext key]
  (apply str (map-indexed (fn [i ch]
                            (char (+ (int \A)
                                     (mod (+ (- (int (nth key (mod i (count key)))) 65)
                                             (- (int ch) 65))
                                          26))))
                          plaintext)))

(defn vernam-decrypt
  "Decrypts Vernam ciphertext with the key."
  [ciphertext key]
  (apply str (map-indexed (fn [i ch]
                            (char (+ (int \A)
                                     (mod (- (int ch)
                                             (int (nth key (mod i (count key)))))
                                          26))))
                          ciphertext)))
