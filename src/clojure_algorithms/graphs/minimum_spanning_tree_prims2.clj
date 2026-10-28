(ns clojure-algorithms.graphs.minimum-spanning-tree-prims2
  "Shortest-path tree via a priority queue (the original's prims_algo
   relaxes dist[node] + weight, i.e. Dijkstra's algorithm, kept here).")

(defn make-graph
  "Builds an empty undirected weighted graph."
  [] {:connections {}})

(defn add-node [graph node]
  (if (contains? (:connections graph) node)
    graph (assoc-in graph [:connections node] {})))

(defn add-edge
  "Returns the graph with the node1<->node2 weighted edge added."
  [graph node1 node2 weight]
  (-> graph
      (add-node node1) (add-node node2)
      (assoc-in [:connections node1 node2] weight)
      (assoc-in [:connections node2 node1] weight)))

(defn prims-algo
  "Returns [dist parent] maps from an arbitrary start node."
  [graph]
  (let [conns (:connections graph)]
    (if (empty? conns)
      [{} {}]
      (let [start (first (keys conns))]
        (loop [heap (sorted-set [0 start])
               dist {start 0}
               parent {start nil}
               done #{}]
          (if (empty? heap)
            [dist parent]
            (let [[d node] (first heap)
                  heap (disj heap [d node])]
              (if (contains? done node)
                (recur heap dist parent done)
                (let [done (conj done node)
                      [heap dist parent]
                      (reduce (fn [[h ds par] [nbr w]]
                                (let [nd (+ d w)]
                                  (if (< nd (get ds nbr Long/MAX_VALUE))
                                    [(conj h [nd nbr]) (assoc ds nbr nd)
                                     (assoc par nbr node)]
                                    [h ds par])))
                              [heap dist parent]
                              (get conns node))]
                  (recur heap dist parent done))))))))))
