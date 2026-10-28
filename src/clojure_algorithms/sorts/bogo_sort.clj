(ns clojure-algorithms.sorts.bogo-sort
  "Bogosort (permutation sort): shuffles until the collection is sorted.
  Deliberately inefficient; for tiny inputs only.
  Ported from TheAlgorithms/Python.")

(defn- sorted-asc?
  [v]
  (or (< (count v) 2)
      (every? (fn [[a b]] (not (neg? (compare b a))))
              (partition 2 1 v))))

(defn bogo-sort
  "Shuffles `coll` until it is sorted ascending. Returns a sorted vector."
  [coll]
  (loop [v (vec coll)]
    (if (sorted-asc? v)
      v
      (recur (vec (shuffle v))))))
