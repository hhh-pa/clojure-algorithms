(ns clojure-algorithms.ciphers.polybius
  "Polybius square cipher (numpy-free)."
  (:require [clojure.string :as str]))

(def square
  [[\a \b \c \d \e] [\f \g \h \i \k] [\l \m \n \o \p]
   [\q \r \s \t \u] [\v \w \x \y \z]])

(def ^:private pos
  (into {} (for [r (range 5) c (range 5)] [(get-in square [r c]) [(inc r) (inc c)]])))

(defn letter-to-numbers
  "Returns the 1-based [row col] of a letter."
  [letter]
  (pos letter))

(defn numbers-to-letter
  "Returns the letter at 1-based [row col]."
  [index1 index2]
  (get-in square [(dec index1) (dec index2)]))

(defn encode
  "Encodes a message to digit pairs."
  [message]
  (let [msg (-> message str/lower-case (str/replace "j" "i"))]
    (apply str (map (fn [ch]
                      (if (= ch \space)
                        " "
                        (let [[a b] (letter-to-numbers ch)] (str a b))))
                    msg))))

(defn decode
  "Decodes digit pairs back to a message."
  [message]
  (let [msg (str/replace message " " "  ")]
    (loop [i 0 out []]
      (if (>= i (count msg))
        (apply str out)
        (if (= (nth msg i) \space)
          (recur (+ i 2) (conj out \space))
          (recur (+ i 2) (conj out (numbers-to-letter
                                    (Character/digit (nth msg i) 10)
                                    (Character/digit (nth msg (inc i)) 10)))))))))
