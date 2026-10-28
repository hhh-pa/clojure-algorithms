(ns clojure-algorithms.ciphers.playfair-cipher
  "Playfair cipher."
  (:require [clojure.string :as str]))

(defn prepare-input
  "Upcases, strips non-letters, splits doubles with X, pads to even length."
  [dirty]
  (let [s (apply str (filter #(Character/isLetter ^char %) (str/upper-case dirty)))]
    (if (< (count s) 2)
      s
      (let [clean (apply str (mapcat (fn [[a b]] (if (= a b) [a \X] [a]))
                                     (map vector s (rest s))))]
        (str clean (last s)
             (when (odd? (inc (count clean))) "X"))))))

(defn generate-table
  "Builds the 25-letter key table (I/J merged)."
  [key]
  (let [alphabet "ABCDEFGHIKLMNOPQRSTUVWXYZ"
        from-key (distinct (filter #(>= (.indexOf ^String alphabet (str %)) 0)
                                   (str/upper-case key)))]
    (vec (concat from-key (remove (set from-key) alphabet)))))

(defn encode
  "Encodes plaintext with the Playfair cipher."
  [plaintext key]
  (let [table (generate-table key)
        prepared (str/replace (prepare-input plaintext) "J" "I")]
    (apply str (mapcat (fn [[c1 c2]]
                         (let [i1 (.indexOf ^java.util.List table c1)
                               i2 (.indexOf ^java.util.List table c2)
                               r1 (quot i1 5) col1 (mod i1 5)
                               r2 (quot i2 5) col2 (mod i2 5)]
                           (cond (= r1 r2) [(nth table (+ (* r1 5) (mod (inc col1) 5)))
                                            (nth table (+ (* r2 5) (mod (inc col2) 5)))]
                                 (= col1 col2) [(nth table (+ (* (mod (inc r1) 5) 5) col1))
                                                (nth table (+ (* (mod (inc r2) 5) 5) col2))]
                                 :else [(nth table (+ (* r1 5) col2))
                                        (nth table (+ (* r2 5) col1))])))
                       (partition 2 prepared)))))

(defn decode
  "Decodes Playfair ciphertext."
  [ciphertext key]
  (let [table (generate-table key)]
    (apply str (mapcat (fn [[c1 c2]]
                         (let [i1 (.indexOf ^java.util.List table c1)
                               i2 (.indexOf ^java.util.List table c2)
                               r1 (quot i1 5) col1 (mod i1 5)
                               r2 (quot i2 5) col2 (mod i2 5)]
                           (cond (= r1 r2) [(nth table (+ (* r1 5) (mod (dec col1) 5)))
                                            (nth table (+ (* r2 5) (mod (dec col2) 5)))]
                                 (= col1 col2) [(nth table (+ (* (mod (dec r1) 5) 5) col1))
                                                (nth table (+ (* (mod (dec r2) 5) 5) col2))]
                                 :else [(nth table (+ (* r1 5) col2))
                                        (nth table (+ (* r2 5) col1))])))
                       (partition 2 ciphertext)))))
