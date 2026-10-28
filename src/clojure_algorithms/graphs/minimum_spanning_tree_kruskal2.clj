(ns clojure-algorithms.graphs.minimum-spanning-tree-kruskal2
  "Minimum spanning tree over a weighted undirected graph (Kruskal).")

(defn make-graph
  "Builds an empty undirected weighted graph."
  [] {:connections {}})

(defn add-node [graph node]
  (if (contains? (:connections graph) node)
    graph (assoc-in graph [:connections node] {})))

(defn add-edge
  "Returns the graph with the weighted node1<->node2 edge added."
  [graph node1 node2 weight]
  (-> graph
      (add-node node1) (add-node node2)
      (assoc-in [:connections node1 node2] weight)
      (assoc-in [:connections node2 node1] weight)))

(defn kruskal
  "Returns the MST as a new graph."
  [graph]
  (let [conns (:connections graph)
        seen (atom #{})
        edges (atom [])]
    (doseq [[s ns] conns [e _] ns]
      (when-not (contains? @seen [s e])
        (swap! seen conj [e s])
        (swap! edges conj [s e (get-in conns [s e])])))
    (let [edges (sort-by #(nth % 2) @edges)
          parent (atom (into {} (map #(vector % %) (keys conns))))
          find (fn find [x]
                 (let [p (get @parent x)]
                   (if (= p x) x (let [r (find p)]
                                   (swap! parent assoc x r) r))))]
      (loop [mst (make-graph) n 0 i 0]
        (if (or (>= n (dec (count conns))) (>= i (count edges)))
          mst
          (let [[u v w] (nth edges i)
                pu (find u) pv (find v)]
            (if (= pu pv)
              (recur mst n (inc i))
              (do (swap! parent assoc pu pv)
                  (recur (add-edge mst u v w) (inc n) (inc i))))))))))
