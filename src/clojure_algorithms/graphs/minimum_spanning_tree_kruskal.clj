(ns clojure-algorithms.graphs.minimum-spanning-tree-kruskal
  "Minimum spanning tree via Kruskal's algorithm with union-find.")

(defn kruskal
  "Returns the MST edges from [u v weight] edges over num-nodes nodes."
  [num-nodes edges]
  (let [parent (atom (vec (range num-nodes)))
        find (fn find [i]
               (if (= i (nth @parent i))
                 i
                 (let [r (find (nth @parent i))]
                   (swap! parent assoc i r)
                   r)))]
    (reduce (fn [mst [u v w :as edge]]
              (let [pu (find u) pv (find v)]
                (if (= pu pv)
                  mst
                  (do (swap! parent assoc pu pv)
                      (conj mst edge)))))
            [] (sort-by #(nth % 2) edges))))
