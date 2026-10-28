(ns clojure-algorithms.data-structures.arrays.equilibrium-index-in-array
  "Port of data_structures/arrays/equilibrium_index_in_array.py.
  Finds the index where left and right sums are equal.")

(defn equilibrium-index
  "First index I where (sum of items before I) equals (sum after I), else -1."
  [arr]
  (let [total (reduce + 0 arr)]
    (loop [i 0 left 0]
      (cond
        (>= i (count arr)) -1
        (= left (- total left (nth arr i))) i
        :else (recur (inc i) (+ left (nth arr i)))))))
