(ns clojure-algorithms.graphs.depth-first-search-2
  "Depth-first traversal over an explicit graph map, iterative and recursive.")

(defn make-graph
  "Builds an empty directed graph."
  [] {})

(defn add-edge
  "Returns the graph with edge from->to added."
  [graph from-vertex to-vertex]
  (-> graph
      (update from-vertex (fnil conj []) to-vertex)
      (update to-vertex #(or % []))))

(defn dfs
  "Returns DFS visit order covering every vertex of the graph."
  [graph]
  (let [visit (fn [start visited order]
                (loop [stack [start] visited visited order order]
                  (if (empty? stack)
                    [visited order]
                    (let [v (peek stack) s (pop stack)]
                      (if (contains? visited v)
                        (recur s visited order)
                        (recur (into s (get graph v []))
                               (conj visited v) (conj order v)))))))]
    (second (reduce (fn [[visited order] v]
                      (if (contains? visited v)
                        [visited order]
                        (visit v visited order)))
                    [#{} []] (sort (keys graph))))))

(defn dfs-recursive
  "Returns DFS visit order from start-vertex (recursive)."
  [graph start-vertex]
  (let [seen (atom #{})]
    (letfn [(visit [v]
              (when-not (contains? @seen v)
                (swap! seen conj v)
                (into [v] (mapcat visit (get graph v [])))))]
      (vec (visit start-vertex)))))
