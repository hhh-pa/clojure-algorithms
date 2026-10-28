(ns clojure-algorithms.sorts.recursive-quick-sort
  "Recursive quicksort pivoting on the first element.
  Ported from TheAlgorithms/Python.")

(defn quick-sort
  "Sorts `coll` with recursive quicksort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [pivot (first v)
            rest-v (subvec v 1)]
        (into (into (quick-sort
                     (filterv #(not (pos? (compare % pivot))) rest-v))
                    [pivot])
              (quick-sort
               (filterv #(pos? (compare % pivot)) rest-v)))))))
