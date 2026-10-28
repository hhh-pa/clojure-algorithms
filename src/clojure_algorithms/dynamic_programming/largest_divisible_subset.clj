(ns clojure-algorithms.dynamic-programming.largest-divisible-subset
  "Largest subset where every pair (x, y) has x | y or y | x.
  Ported from TheAlgorithms/Python
  dynamic_programming/largest_divisible_subset.py.")

(defn largest-divisible-subset
  "A largest divisible subset of `items`, largest element first."
  [items]
  (let [sorted-items (vec (sort items))
        n (count sorted-items)]
    (if (zero? n)
      []
      (let [[memo hashed] (reduce (fn [[memo hashed] i]
                                    (reduce (fn [[m h] prev]
                                              (if (and (or (zero? (sorted-items prev))
                                                           (zero? (mod (sorted-items i)
                                                                       (sorted-items prev))))
                                                       (> (inc (m prev)) (m i)))
                                                [(assoc m i (inc (m prev)))
                                                 (assoc h i prev)]
                                                [m h]))
                                            [memo hashed]
                                            (range i)))
                                  [(vec (repeat n 1)) (vec (range n))]
                                  (range n))
            last-index (reduce (fn [best i]
                                 (if (> (memo i) (memo best)) i best))
                               0 (range n))]
        (loop [idx last-index acc []]
          (if (= (hashed idx) idx)
            (conj acc (sorted-items idx))
            (recur (hashed idx) (conj acc (sorted-items idx)))))))))
