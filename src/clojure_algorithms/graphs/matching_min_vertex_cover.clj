(ns clojure-algorithms.graphs.matching-min-vertex-cover
  "Matching-based 2-approximation for minimum vertex cover.")

(defn get-edges
  "Returns the set of [from to] edge pairs."
  [graph]
  (set (for [[from tos] graph to tos] [from to])))

(defn matching-min-vertex-cover
  "Takes both endpoints of an uncovered edge until all are covered."
  [graph]
  (loop [edges (sort (get-edges graph)) chosen #{}]
    (if (empty? edges)
      chosen
      (let [[from to] (last edges)
            chosen (conj chosen from to)]
        (recur (remove #(or (contains? chosen (first %))
                            (contains? chosen (second %)))
                       edges)
               chosen)))))
