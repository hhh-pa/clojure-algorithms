(ns clojure-algorithms.data-structures.arrays.merge-intervals
  "Port of data_structures/arrays/merge_intervals.py.
  Merges overlapping intervals.")

(defn merge-intervals
  "Merge overlapping [start end] INTERVALS."
  [intervals]
  (if (empty? intervals)
    []
    (let [sorted-ivs (vec (sort-by first intervals))]
      (loop [merged [(first sorted-ivs)] rest-ivs (rest sorted-ivs)]
        (if (empty? rest-ivs)
          merged
          (let [cur (peek merged)
                [s e] (first rest-ivs)]
            (if (<= s (second cur))
              (recur (conj (pop merged) [(first cur) (max (second cur) e)])
                     (rest rest-ivs))
              (recur (conj merged [(first (first rest-ivs))
                                   (second (first rest-ivs))])
                     (rest rest-ivs)))))))))
