(ns clojure-algorithms.sorts.pigeonhole-sort
  "Pigeonhole sort for integer collections.
  Ported from TheAlgorithms/Python.")

(defn pigeonhole-sort
  "Sorts integer `coll` with pigeonhole sort. Returns a sorted vector.
  Throws AssertionError when any element is not an integer."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (do
        (doseq [x v]
          (assert (integer? x) "integers only please"))
        (let [lo (apply min v)
              holes (long-array (inc (- (apply max v) lo)))]
          (doseq [x v]
            (aset holes (- x lo) (inc (aget holes (- x lo)))))
          (vec (mapcat (fn [c] (repeat (aget holes (- c lo)) c))
                       (range lo (inc (apply max v))))))))))
