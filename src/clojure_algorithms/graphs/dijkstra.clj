(ns clojure-algorithms.graphs.dijkstra
  "Dijkstra shortest-path cost over string-keyed adjacency lists.
   Graph maps node -> [[neighbour cost] ...].")

(defn dijkstra
  "Returns the cheapest start->end cost, or -1 when unreachable."
  [graph start end]
  (loop [heap (sorted-set [0 start]) visited #{}]
    (if (empty? heap)
      -1
      (let [[cost v] (first heap)
            heap (disj heap [cost v])]
        (cond
          (= v end) cost
          (contains? visited v) (recur heap visited)
          :else (recur (into heap (map (fn [[n c]] [(+ cost c) n])
                                       (get graph v [])))
                       (conj visited v)))))))
