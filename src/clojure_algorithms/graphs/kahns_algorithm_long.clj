(ns clojure-algorithms.graphs.kahns-algorithm-long
  "Longest path length in a DAG via Kahn's algorithm.")

(defn longest-distance
  "Returns the longest path node count from any source."
  [graph]
  (let [n (count graph)
        indegree (atom (vec (repeat n 0)))
        long-dist (atom (vec (repeat n 1)))]
    (doseq [vs (vals graph) v vs]
      (swap! indegree update v inc))
    (loop [queue (vec (filter #(zero? (nth @indegree %)) (range n)))]
      (if (empty? queue)
        (apply max @long-dist)
        (let [v (first queue)]
          (doseq [x (get graph v [])]
            (swap! indegree update x dec)
            (swap! long-dist assoc x
                   (max (nth @long-dist x) (inc (nth @long-dist v)))))
          (recur (vec (concat (rest queue)
                              (filter #(zero? (nth @indegree %))
                                      (get graph v []))))))))))
