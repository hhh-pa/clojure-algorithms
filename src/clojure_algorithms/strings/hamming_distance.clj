(ns clojure-algorithms.strings.hamming-distance
  "Hamming distance between equal-length strings.
  Ported from TheAlgorithms/Python.")

(defn hamming-distance
  "Number of positions at which `a` and `b` differ.
  Throws IllegalArgumentException when lengths differ."
  [a b]
  (when-not (= (count a) (count b))
    (throw (IllegalArgumentException. "String lengths must match!")))
  (count (filter false? (map = a b))))
