(ns clojure-algorithms.ciphers.gronsfeld-cipher
  "Gronsfeld cipher (numeric-key Vigenere)."
  (:require [clojure.string :as str]))

(def ^:private alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn gronsfeld
  "Encrypts text with a numeric key string."
  [text key]
  (when (empty? key)
    (throw (ArithmeticException. "division by zero")))
  (let [keys (mapv #(Character/digit ^char % 10) key)
        upper (str/upper-case text)]
    (apply str (map-indexed
                (fn [i ch]
                  (let [pos (.indexOf ^String alpha (str ch))]
                    (if (neg? pos)
                      ch
                      (nth alpha (mod (+ pos (nth keys (mod i (count keys))))
                                      (count alpha))))))
                upper))))
