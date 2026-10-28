(ns clojure-algorithms.dynamic-programming.subset-generation
  "n-element combinations of a collection via DP.
  Ported from TheAlgorithms/Python dynamic_programming/subset_generation.py.")

(defn subset-combinations
  "Sorted vector of n-element combination vectors from `elements`."
  [elements n]
  (let [elems (vec elements)
        r (count elems)]
    (if (> n r)
      []
      (let [dp (loop [i 1 table (assoc (vec (repeat (inc r) [])) 0 [[]])]
                 (if (> i r)
                   table
                   (recur (inc i)
                          (reduce (fn [t j]
                                    (update t j into
                                            (map #(conj % (elems (dec i)))
                                                 (t (dec j)))))
                                  table
                                  (range i 0 -1)))))]
        (try
          (vec (sort (dp n)))
          (catch ClassCastException _ (vec (dp n))))))))
