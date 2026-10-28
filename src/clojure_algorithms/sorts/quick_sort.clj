(ns clojure-algorithms.sorts.quick-sort
  "Functional quicksort with a random pivot.
  Ported from TheAlgorithms/Python.")

(defn quick-sort
  "Sorts `coll` with quicksort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (< (count v) 2)
      v
      (let [pivot (nth v (rand-int (count v)))]
        (into (into (quick-sort (filterv #(neg? (compare % pivot)) v))
                    (filterv #(zero? (compare % pivot)) v))
              (quick-sort (filterv #(pos? (compare % pivot)) v)))))))
