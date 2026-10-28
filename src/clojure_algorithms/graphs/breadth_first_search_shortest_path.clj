(ns clojure-algorithms.graphs.breadth-first-search-shortest-path
  "Shortest path in an unweighted graph via a BFS parent map.")

(defn bfs-parents
  "Returns the parent map of the BFS tree rooted at source."
  [graph source]
  (loop [queue [source] visited #{source} parent {source nil}]
    (if (empty? queue)
      parent
      (let [v (first queue)
            fresh (remove visited (get graph v []))]
        (recur (into (subvec queue 1) fresh)
               (into visited fresh)
               (into parent (map #(vector % v) fresh)))))))

(defn shortest-path
  "Returns the source->target path as a \"v1->v2->...\" string.
   Throws when no path exists."
  [graph source target]
  (if (= target source)
    source
    (let [parent (bfs-parents graph source)]
      (if (not (contains? parent target))
        (throw (IllegalArgumentException.
                (str "No path from vertex: " source " to vertex: " target)))
        (loop [v target path (list target)]
          (let [p (get parent v)]
            (if (= p source)
              (clojure.string/join "->" (cons source path))
              (recur p (cons p path)))))))))
