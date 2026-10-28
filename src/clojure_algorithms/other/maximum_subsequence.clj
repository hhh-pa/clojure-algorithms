(ns clojure-algorithms.other.maximum-subsequence
  "Maximum-sum non-empty contiguous subsequence (Kadane's algorithm).

  Port of TheAlgorithms/Python `other/maximum_subsequence.py`.
  The interactive stdin demo from `__main__` is not ported.")

(defn max-subsequence-sum
  "Return the maximum sum over all non-empty contiguous subsequences
  of `nums`. Throws on empty input."
  [nums]
  (when (empty? nums)
    (throw (IllegalArgumentException. "Input sequence should not be empty")))
  (reduce (fn [best num] (max best (+ best num) num))
          (first nums)
          (rest nums)))
