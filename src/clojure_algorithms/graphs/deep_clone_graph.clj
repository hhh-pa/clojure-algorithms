(ns clojure-algorithms.graphs.deep-clone-graph
  "Deep clone of an undirected graph held as an adjacency map.
   Clojure data is immutable, so the clone is a structurally rebuilt map.")

(defn clone-graph
  "Returns a fresh copy of the adjacency map reachable from node.
   Returns nil for a nil node."
  [graph node]
  (when node
    (loop [stack [node] seen #{} acc {}]
      (if (empty? stack)
        acc
        (let [v (peek stack) s (pop stack)]
          (if (contains? seen v)
            (recur s seen acc)
            (let [neighbours (vec (get graph v []))]
              (recur (into s (remove seen neighbours))
                     (conj seen v)
                     (assoc acc v neighbours)))))))))
