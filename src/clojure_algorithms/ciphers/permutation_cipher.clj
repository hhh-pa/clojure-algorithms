(ns clojure-algorithms.ciphers.permutation-cipher
  "Permutation (block transposition) cipher."
  (:require [clojure.string :as str]))

(defn generate-valid-block-size
  "Picks a random divisor of message-length greater than 1."
  [message-length]
  (rand-nth (filterv #(zero? (mod message-length %))
                     (range 2 (inc message-length)))))

(defn generate-permutation-key
  "Returns a random permutation of (range block-size)."
  [block-size]
  (vec (shuffle (range block-size))))

(defn encrypt
  "Encrypts message, returning [ciphertext key]."
  ([message]
   (let [block-size (generate-valid-block-size (count message))]
     (encrypt message (generate-permutation-key block-size) block-size)))
  ([message key block-size]
   (let [message (str/upper-case message)]
     [(apply str (mapcat (fn [i]
                           (let [block (subs message i (+ i block-size))]
                             (map #(nth block %) key)))
                         (range 0 (count message) block-size)))
      key])))

(defn decrypt
  "Decrypts ciphertext with the permutation key."
  [encrypted-message key]
  (let [n (count key)]
    (apply str (mapcat (fn [i]
                         (let [block (subs encrypted-message i (+ i n))
                               original (object-array n)]
                           (doseq [[j d] (map-indexed vector key)]
                             (aset original d (nth block j)))
                           (seq original)))
                       (range 0 (count encrypted-message) n)))))
