(ns clojure-algorithms.graphs.breadth-first-search-shortest-path-2
  "BFS shortest path and distance in an unweighted graph.")

(defn bfs-shortest-path
  "Returns the start->goal path, or [] when goal is unreachable."
  [graph start goal]
  (if (= start goal)
    [start]
    (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start])
           visited #{start}]
      (if (empty? queue)
        []
        (let [path (peek queue)
              v (peek path)]
          (if (= v goal)
            path
            (let [fresh (remove visited (get graph v []))]
              (recur (into (pop queue) (map #(conj path %) fresh))
                     (into visited fresh)))))))))

(defn bfs-shortest-path-distance
  "Returns the start->target distance; 0 when equal, -1 when unreachable."
  [graph start target]
  (cond (not (contains? graph target)) -1
        (= start target) 0
        :else (let [path (bfs-shortest-path graph start target)]
                (if (empty? path) -1 (dec (count path))))))
