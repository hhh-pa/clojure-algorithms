(ns clojure-algorithms.greedy-methods.smallest-range
  "Smallest range covering at least one number from each of k sorted lists,
  found with a min-heap."
  (:import (java.util PriorityQueue Comparator)))

(defn smallest-range
  "Returns the smallest [lo hi] range including one element from each list."
  [nums]
  (let [lists (mapv vec nums)
        cmp (reify Comparator
              (compare [_ a b] (compare (first a) (first b))))
        heap (PriorityQueue. ^Comparator cmp)]
    (doseq [[i items] (map-indexed vector lists)]
      (.add heap [(first items) i 0]))
    (loop [current-max (apply max (map first lists))
           best nil
           best-size nil]
      (if (.isEmpty heap)
        best
        (let [[current-min list-idx elem-idx] (.poll heap)
              size (- current-max current-min)
              [best best-size] (if (or (nil? best-size) (< size best-size))
                                 [[current-min current-max] size]
                                 [best best-size])]
          (if (= elem-idx (dec (count (nth lists list-idx))))
            best
            (let [next-elem (nth (nth lists list-idx) (inc elem-idx))]
              (.add heap [next-elem list-idx (inc elem-idx)])
              (recur (max current-max next-elem) best best-size))))))))
