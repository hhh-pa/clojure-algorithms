(ns clojure-algorithms.graphs.breadth-first-search
  "Breadth-first traversal over an adjacency-map graph.")

(defn make-graph
  "Builds an empty directed graph."
  [] {})

(defn add-edge
  "Returns the graph with edge from->to added."
  [graph from-vertex to-vertex]
  (-> graph
      (update from-vertex (fnil conj #{}) to-vertex)
      (update to-vertex #(or % #{}))))

(defn bfs
  "Returns the set of vertices reachable from start-vertex in BFS order."
  [graph start-vertex]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY start-vertex)
         visited #{start-vertex}]
    (if (empty? queue)
      visited
      (let [v (peek queue)
            neighbours (sort (remove visited (get graph v #{})))]
        (recur (into (pop queue) neighbours)
               (into visited neighbours))))))
