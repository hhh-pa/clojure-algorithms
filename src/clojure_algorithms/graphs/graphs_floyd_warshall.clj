(ns clojure-algorithms.graphs.graphs-floyd-warshall
  "All-pairs shortest paths via Floyd-Warshall.")

(def inf "Infinity marker." Double/POSITIVE_INFINITY)

(defn floyd-warshall
  "Returns [dist v] shortest-path matrix over the v-vertex weight matrix."
  [graph v]
  (let [dist (atom (mapv vec graph))]
    (doseq [k (range v) i (range v) j (range v)]
      (let [ik (get-in @dist [i k])
            kj (get-in @dist [k j])
            ij (get-in @dist [i j])]
        (when (and (not= ik inf) (not= kj inf) (< (+ ik kj) ij))
          (swap! dist assoc-in [i j] (+ ik kj)))))
    [@dist v]))
