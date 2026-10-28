(ns clojure-algorithms.graphs.g-topological-sort
  "Topological sort via DFS finishing order.")

(defn topological-sort
  "Returns a topological ordering of the adjacency-vector graph."
  [graph]
  (let [visited (atom (vec (repeat (count graph) false)))
        stack (atom [])]
    (letfn [(dfs [u]
              (swap! visited assoc u true)
              (doseq [v (nth graph u)]
                (when-not (nth @visited v)
                  (dfs v)))
              (swap! stack conj u))]
      (doseq [v (range (count graph))]
        (when-not (nth @visited v)
          (dfs v))))
    (vec (reverse @stack))))
