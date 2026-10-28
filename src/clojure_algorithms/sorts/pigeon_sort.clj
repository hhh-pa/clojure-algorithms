(ns clojure-algorithms.sorts.pigeon-sort
  "Pigeon hole sort for integer collections.
  Ported from TheAlgorithms/Python.")

(defn pigeon-sort
  "Sorts integer `coll` with pigeon hole sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (empty? v)
      []
      (let [lo (apply min v)
            hi (apply max v)
            reps (long-array (inc (- hi lo)))]
        (doseq [x v]
          (aset reps (- x lo) (inc (aget reps (- x lo)))))
        (vec (mapcat (fn [c] (repeat (aget reps (- c lo)) c))
                     (range lo (inc hi))))))))
