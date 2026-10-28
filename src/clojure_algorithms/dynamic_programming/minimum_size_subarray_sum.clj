(ns clojure-algorithms.dynamic-programming.minimum-size-subarray-sum
  "Length of the shortest contiguous subarray summing to at least `target`
  (sliding window; 0 when none exists).
  Ported from TheAlgorithms/Python
  dynamic_programming/minimum_size_subarray_sum.py.")

(defn minimum-subarray-sum
  "Shortest length with sum >= `target`, or 0."
  [target numbers]
  (cond
    (empty? numbers) 0
    (and (zero? target) (some zero? numbers)) 0
    (not (and (sequential? numbers) (every? integer? numbers)))
    (throw (IllegalArgumentException. "numbers must be an iterable of integers"))
    :else
    (do
      (let [arr (vec numbers)]
        (loop [left 0 right 0 curr-sum 0 best Long/MAX_VALUE]
          (if (>= right (count arr))
            (if (= best Long/MAX_VALUE) 0 best)
            (let [curr-sum (+ curr-sum (arr right))
                  [left curr-sum best]
                  (loop [l left s curr-sum b best]
                    (if (and (>= s target) (<= l right))
                      (recur (inc l) (- s (arr l)) (min b (inc (- right l))))
                      [l s b]))]
              (recur left (inc right) curr-sum best))))))))
