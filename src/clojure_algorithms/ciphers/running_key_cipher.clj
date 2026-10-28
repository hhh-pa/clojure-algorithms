(ns clojure-algorithms.ciphers.running-key-cipher
  "Running key cipher."
  (:require [clojure.string :as str]))

(defn- prep [s] (str/replace (str/upper-case s) " " ""))

(defn running-key-encrypt
  "Encrypts plaintext with a running key."
  [key plaintext]
  (let [pt (prep plaintext) k (prep key) n (count k)]
    (apply str (map-indexed (fn [i ch]
                              (char (+ (int \A)
                                       (mod (+ (- (int ch) (int \A))
                                               (- (int (nth k (mod i n))) (int \A)))
                                            26))))
                            pt))))

(defn running-key-decrypt
  "Decrypts ciphertext with a running key."
  [key ciphertext]
  (let [ct (prep ciphertext) k (prep key) n (count k)]
    (apply str (map-indexed (fn [i ch]
                              (char (+ (int \A)
                                       (mod (- (- (int ch) (int \A))
                                               (- (int (nth k (mod i n))) (int \A)))
                                            26))))
                            ct))))
