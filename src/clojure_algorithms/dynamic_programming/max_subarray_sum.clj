(ns clojure-algorithms.dynamic-programming.max-subarray-sum
  "Maximum subarray sum via Kadane's algorithm, O(n) time, O(1) space.
  Ported from TheAlgorithms/Python dynamic_programming/max_subarray_sum.py.")

(defn max-subarray-sum
  "Maximum contiguous sum of `arr`. With `allow-empty?`, 0 for all-negative."
  ([arr] (max-subarray-sum arr false))
  ([arr allow-empty?]
   (if (empty? arr)
     0
     (let [[best _] (reduce (fn [[best curr] num]
                              (let [curr (max (if allow-empty? 0 num)
                                              (+ curr num))]
                                [(max best curr) curr]))
                            [(if allow-empty? 0 Double/NEGATIVE_INFINITY) 0]
                            arr)]
       best))))
