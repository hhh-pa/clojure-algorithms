(ns clojure-algorithms.graphs.check-bipatrite
  "Bipartite checking via DFS and BFS two-colouring.")

(defn- colour-component
  "Two-colours the component of start; returns colour map or nil on clash.
   lifo? selects DFS (stack) vs BFS (queue) traversal."
  [graph start lifo?]
  (let [empty-q (if lifo? [] clojure.lang.PersistentQueue/EMPTY)]
    (loop [queue (conj empty-q start) colours {start 0}]
      (if (empty? queue)
        colours
        (let [node (peek queue)
              c (get colours node)
              neighbours (get graph node [])]
          (if (some #(= (get colours % ::none) c) neighbours)
            nil
            (let [fresh (remove #(contains? colours %) neighbours)]
              (recur (into (pop queue) fresh)
                     (into colours (map #(vector % (- 1 c)) fresh))))))))))

(defn- bipartite?
  [graph lifo?]
  (loop [nodes (keys graph) colours {}]
    (if (empty? nodes)
      true
      (let [n (first nodes)]
        (if (contains? colours n)
          (recur (rest nodes) colours)
          (let [sub (colour-component graph n lifo?)]
            (if (nil? sub)
              false
              (recur (rest nodes) (merge colours sub)))))))))

(defn is-bipartite-dfs
  "True when the graph is bipartite (DFS colouring)."
  [graph]
  (bipartite? graph true))

(defn is-bipartite-bfs
  "True when the graph is bipartite (BFS colouring)."
  [graph]
  (bipartite? graph false))
