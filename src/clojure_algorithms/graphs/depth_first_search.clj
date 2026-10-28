(ns clojure-algorithms.graphs.depth-first-search
  "Iterative depth-first traversal over an adjacency-map graph.")

(defn depth-first-search
  "Returns the set of vertices reachable from start."
  [graph start]
  (loop [stack [start] explored #{}]
    (if (empty? stack)
      explored
      (let [v (peek stack) rest-stack (pop stack)]
        (if (contains? explored v)
          (recur rest-stack explored)
          (recur (into rest-stack (get graph v []))
                 (conj explored v)))))))
