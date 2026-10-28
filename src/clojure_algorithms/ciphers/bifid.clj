(ns clojure-algorithms.ciphers.bifid
  "Bifid cipher over a Polybius square (numpy-free)."
  (:require [clojure.string :as str]))

(def square
  [[\a \b \c \d \e] [\f \g \h \i \k] [\l \m \n \o \p]
   [\q \r \s \t \u] [\v \w \x \y \z]])

(def ^:private pos
  (into {} (for [r (range 5) c (range 5)] [(get-in square [r c]) [(inc r) (inc c)]])))

(defn letter-to-numbers
  "Returns the 1-based [row col] of a letter in the square."
  [letter]
  (pos letter))

(defn numbers-to-letter
  "Returns the letter at 1-based [row col] in the square."
  [index1 index2]
  (get-in square [(dec index1) (dec index2)]))

(defn- clean [message] (-> message str/lower-case (str/replace " " "") (str/replace "j" "i")))

(defn encode
  "Encodes a message with the Bifid cipher."
  [message]
  (let [msg (clean message)
        rows (mapv #(first (letter-to-numbers %)) msg)
        cols (mapv #(second (letter-to-numbers %)) msg)
        flat (vec (concat rows cols))]
    (apply str (map (fn [i] (numbers-to-letter (nth flat (* 2 i))
                                               (nth flat (inc (* 2 i)))))
                    (range (count msg))))))

(defn decode
  "Decodes a Bifid ciphertext."
  [message]
  (let [msg (str/lower-case message)
        flat (vec (mapcat (fn [ch] (let [[a b] (letter-to-numbers ch)] [a b])) msg))
        n (count msg)
        rows (subvec flat 0 n)
        cols (subvec flat n (* 2 n))]
    (apply str (map numbers-to-letter rows cols))))
