(ns clojure-algorithms.graphs.kahns-algorithm-topo
  "Topological sort of a DAG via Kahn's algorithm (BFS).")

(defn topological-sort
  "Returns a topological ordering, or nil when the graph has a cycle."
  [graph]
  (let [indegree (atom (into {} (map #(vector % 0) (keys graph))))]
    (doseq [vs (vals graph) v vs]
      (swap! indegree update v (fnil inc 0)))
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (filter #(zero? (get @indegree %)) (keys graph)))
           order [] done 0]
      (if (empty? queue)
        (when (= done (count graph)) order)
        (let [v (peek queue)]
          (doseq [nb (get graph v [])]
            (swap! indegree update nb dec))
          (recur (into (pop queue)
                       (filter #(zero? (get @indegree %)) (get graph v [])))
                 (conj order v) (inc done)))))))
