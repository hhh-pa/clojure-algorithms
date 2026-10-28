(ns clojure-algorithms.data-structures.arrays.pairs-with-given-sum
  "Port of data_structures/arrays/pairs_with_given_sum.py.
  Counts index pairs (i < j) whose values sum to the target.")

(defn pairs-with-sum
  "Number of pairs in ARR summing to REQ-SUM."
  [arr req-sum]
  (let [v (vec arr) n (count v)]
    (count (for [i (range n) j (range (inc i) n)
                 :when (= (+ (nth v i) (nth v j)) req-sum)]
             1))))
