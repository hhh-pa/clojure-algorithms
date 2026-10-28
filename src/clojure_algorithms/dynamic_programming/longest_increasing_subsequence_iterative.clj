(ns clojure-algorithms.dynamic-programming.longest-increasing-subsequence-iterative
  "Longest non-decreasing subsequence via iterative DP.
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_increasing_subsequence_iterative.py.")

(defn longest-subsequence
  "Longest non-decreasing subsequence of `array`."
  [array]
  (let [arr (vec array)
        n (count arr)]
    (if (zero? n)
      []
      (let [ends (loop [i 1 table (mapv vector arr)]
                   (if (>= i n)
                     table
                     (recur (inc i)
                            (assoc table i
                                   (reduce (fn [best prev]
                                             (if (and (<= (arr prev) (arr i))
                                                      (> (inc (count (table prev)))
                                                         (count best)))
                                               (conj (table prev) (arr i))
                                               best))
                                           (table i)
                                           (range i))))))]
        (reduce (fn [best cand]
                  (if (> (count cand) (count best)) cand best))
                [] ends)))))
