(ns clojure-algorithms.graphs.scc-kosaraju
  "Strongly connected components via Kosaraju's two-pass algorithm.")

(defn kosaraju
  "Returns the strongly connected components of the n-vertex graph."
  [graph n]
  (let [rev (reduce (fn [r [u vs]]
                      (reduce (fn [r v] (update r v conj u)) r vs))
                    (into {} (map #(vector % []) (range n))) graph)
        visited (atom #{})
        stack (atom [])]
    (letfn [(dfs [u]
              (when-not (contains? @visited u)
                (swap! visited conj u)
                (doseq [v (get graph u [])] (dfs v))
                (swap! stack conj u)))
            (dfs2 [u comp]
              (when-not (contains? @visited u)
                (swap! visited conj u)
                (swap! comp conj u)
                (doseq [v (get rev u [])] (dfs2 v comp))))]
      (doseq [i (range n)] (dfs i))
      (reset! visited #{})
      (reduce (fn [sccs u]
                (if (contains? @visited u)
                  sccs
                  (let [comp (atom [])]
                    (dfs2 u comp)
                    (conj sccs @comp))))
              [] (reverse @stack)))))
