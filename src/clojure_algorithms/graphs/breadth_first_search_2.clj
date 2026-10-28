(ns clojure-algorithms.graphs.breadth-first-search-2
  "Breadth-first traversal returning visit order, list- and queue-based.")

(defn breadth-first-search
  "Returns BFS visit order from start over adjacency-map graph."
  [graph start]
  (loop [queue [start] visited #{start} result []]
    (if (empty? queue)
      result
      (let [v (first queue)
            neighbours (remove visited (get graph v []))]
        (recur (into (subvec queue 1) neighbours)
               (into visited neighbours)
               (conj result v))))))

(defn breadth-first-search-with-deque
  "Queue-based BFS returning visit order from start."
  [graph start]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start)
         visited #{start} result []]
    (if (empty? queue)
      result
      (let [v (peek queue)
            neighbours (remove visited (get graph v []))]
        (recur (into (pop queue) neighbours)
               (into visited neighbours)
               (conj result v))))))
