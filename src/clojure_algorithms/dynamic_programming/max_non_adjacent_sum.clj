(ns clojure-algorithms.dynamic-programming.max-non-adjacent-sum
  "Maximum sum of non-adjacent integers (empty choice allowed, floor 0).
  Ported from TheAlgorithms/Python dynamic_programming/max_non_adjacent_sum.py.")

(defn maximum-non-adjacent-sum
  "Maximum non-adjacent sum of `nums`."
  [nums]
  (if (empty? nums)
    0
    (let [[including excluding] (reduce (fn [[incl excl] num]
                                          [(+ excl num) (max incl excl)])
                                        [(first nums) 0]
                                        (rest nums))]
      (max excluding including))))
