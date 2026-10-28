(ns clojure-algorithms.data-structures.arrays.monotonic-array
  "Port of data_structures/arrays/monotonic_array.py.")

(defn is-monotonic
  "True when NUMS is entirely non-decreasing or non-increasing."
  [nums]
  (let [pairs (map vector nums (rest nums))]
    (or (every? (fn [[a b]] (<= a b)) pairs)
        (every? (fn [[a b]] (>= a b)) pairs))))
