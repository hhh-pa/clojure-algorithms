(ns clojure-algorithms.dynamic-programming.range-sum-query
  "Range-sum queries via prefix sums: O(n) build, O(1) per query.
  Queries are [l r] pairs, 0-based and inclusive.
  Ported from TheAlgorithms/Python dynamic_programming/range_sum_query.py.")

(defn prefix-sum
  "Sums for each [l r] query in `queries` over `array`."
  [array queries]
  (let [prefix (vec (reductions + array))]
    (mapv (fn [[l r]]
            (if (pos? l)
              (- (prefix r) (prefix (dec l)))
              (prefix r)))
          queries)))
