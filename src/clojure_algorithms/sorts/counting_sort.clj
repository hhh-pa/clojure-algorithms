(ns clojure-algorithms.sorts.counting-sort
  "Counting sort (stable) for integer collections, plus a string variant.
  Ported from TheAlgorithms/Python.")

(defn counting-sort
  "Sorts integer `coll` with counting sort (stable).
  Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (let [lo (apply min v)
            counts (reduce (fn [m x] (update m x (fnil inc 0))) {} v)]
        (vec (mapcat (fn [k] (repeat (get counts k 0) k))
                     (range lo (inc (apply max v)))))))))

(defn counting-sort-string
  "Sorts the characters of string `s` with counting sort."
  [s]
  (apply str (map char (counting-sort (map int s)))))
