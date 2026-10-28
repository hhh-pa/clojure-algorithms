(ns clojure-algorithms.graphs.minimum-spanning-tree-prims
  "Minimum spanning tree via Prim's algorithm.")

(defn prisms-algorithm
  "Returns MST edges as [parent vertex] pairs from node 0.
   Adjacency maps vertex -> [[neighbour distance] ...]."
  [adjacency-list]
  (let [n (count adjacency-list)]
    (loop [heap (sorted-set [0 0 -1])
           visited #{}
           dist (assoc (into {} (map #(vector % Long/MAX_VALUE)
                                     (range n))) 0 0)
           nbr (into {} (map #(vector % -1) (range n)))
           tree-edges []]
      (if (or (empty? heap) (= (count visited) n))
        tree-edges
        (let [[d v p] (first heap)
              heap (disj heap [d v p])]
          (if (contains? visited v)
            (recur heap visited dist nbr tree-edges)
            (let [visited (conj visited v)
                  tree-edges (if (neg? p) tree-edges
                                 (conj tree-edges [p v]))
                  [heap dist nbr]
                  (reduce (fn [[h ds nb] [to w]]
                            (if (and (not (contains? visited to))
                                     (< w (get ds to)))
                              [(conj h [w to v]) (assoc ds to w)
                               (assoc nb to v)]
                              [h ds nb]))
                          [heap dist nbr] (get adjacency-list v []))]
              (recur heap visited dist nbr tree-edges))))))))
