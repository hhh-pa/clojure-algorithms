(ns clojure-algorithms.strings.capitalize
  "Capitalize the first letter of a sentence or word.
  Ported from TheAlgorithms/Python.")

(defn capitalize
  "Upper-cases the first character of `s`, leaving the rest untouched."
  [s]
  (if (empty? s)
    ""
    (str (Character/toUpperCase ^char (first s)) (subs s 1))))
