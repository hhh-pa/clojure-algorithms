(ns clojure-algorithms.ciphers.columnar-transposition
  "Columnar transposition cipher."
  (:require [clojure.string :as str]))

(defn- normalize-key
  [key]
  (let [k (apply str (filter #(Character/isLetter ^char %) (str/upper-case key)))]
    (when (empty? k)
      (throw (IllegalArgumentException.
              "Key must be a non-empty alphabetic string")))
    k))

(defn- column-order
  [key]
  (mapv first (sort-by (fn [[i ch]] [ch i]) (map-indexed vector key))))

(defn encrypt
  "Encrypts plaintext under the keyword."
  [plaintext key]
  (let [k (normalize-key key) cols (count k)]
    (if (= 1 cols)
      plaintext
      (let [order (column-order k)
            rows (long (Math/ceil (/ (count plaintext) (double cols))))
            grid (mapv #(subs plaintext (* % cols) (min (* (inc %) cols)
                                                        (count plaintext)))
                       (range rows))]
        (apply str (for [col order r (range rows)
                         :when (< col (count (nth grid r)))]
                     (nth (nth grid r) col)))))))

(defn decrypt
  "Decrypts ciphertext under the keyword."
  [ciphertext key]
  (let [k (normalize-key key) cols (count k)]
    (if (= 1 cols)
      ciphertext
      (let [order (column-order k)
            text-len (count ciphertext)
            rows (long (Math/ceil (/ text-len (double cols))))
            r (mod text-len cols)
            col-lengths (mapv #(if (zero? r) rows (if (< % r) rows (dec rows)))
                              (range cols))
            columns (loop [cols-v (vec (repeat cols "")) idx 0 os order]
                      (if (empty? os)
                        cols-v
                        (let [col (first os) ln (nth col-lengths col)]
                          (recur (assoc cols-v col (subs ciphertext idx (+ idx ln)))
                                 (+ idx ln) (rest os)))))
            pointers (atom (vec (repeat cols 0)))]
        (apply str (for [i (range (* rows cols))
                         :let [c (mod i cols)
                               p (nth @pointers c)
                               col (nth columns c)]
                         :when (< p (count col))]
                     (do (swap! pointers update c inc)
                         (nth col p))))))))
