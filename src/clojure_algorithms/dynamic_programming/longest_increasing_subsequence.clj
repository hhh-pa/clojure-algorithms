(ns clojure-algorithms.dynamic-programming.longest-increasing-subsequence
  "Longest non-decreasing subsequence via recursion.
  Ported from TheAlgorithms/Python
  dynamic_programming/longest_increasing_subsequence.py.")

(defn longest-subsequence
  "Longest non-decreasing subsequence of `array`."
  [array]
  (let [arr (vec array)
        n (count arr)]
    (if (<= n 1)
      arr
      (let [pivot (arr 0)
            longest-subseq (loop [i 1 best []]
                             (if (>= i n)
                               best
                               (if (< (arr i) pivot)
                                 (let [candidate (longest-subsequence
                                                  (subvec arr i))]
                                   (if (> (count candidate) (count best))
                                     candidate
                                     best))
                                 (recur (inc i) best))))
            with-pivot (into [pivot]
                             (longest-subsequence
                              (filterv #(>= % pivot) (subvec arr 1))))]
        (if (> (count with-pivot) (count longest-subseq))
          with-pivot
          longest-subseq)))))
