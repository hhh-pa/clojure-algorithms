(ns clojure-algorithms.matrix.transitive-closure
  "Transitive closure of a directed graph via Floyd-Warshall.
  https://en.wikipedia.org/wiki/Transitive_closure#In_graph_theory")

(defn transitive-closure
  "Returns the transitive closure of the adjacency matrix graph."
  [graph]
  (let [width (count graph)
        ans (vec (map-indexed (fn [i row] (assoc (vec row) i 1)) graph))]
    (reduce (fn [ans k]
              (reduce (fn [ans i]
                        (reduce (fn [ans j]
                                  (if (and (= 1 (get-in ans [i k]))
                                           (= 1 (get-in ans [k j])))
                                    (assoc-in ans [i j] 1)
                                    ans))
                                ans
                                (range width)))
                      ans
                      (range width)))
            ans
            (range width))))
