(ns clojure-algorithms.graphs.connected-components
  "Connected components of an undirected graph via DFS.")

(defn dfs
  "Returns the vertices in vert's component, in DFS pre-order."
  [graph vert]
  (let [seen (atom #{})]
    (letfn [(visit [v]
              (when-not (contains? @seen v)
                (swap! seen conj v)
                (into [v] (mapcat visit (remove @seen (get graph v []))))))]
      (vec (visit vert)))))

(defn connected-components
  "Returns the list of connected components (vertex lists)."
  [graph]
  (let [seen (atom #{})]
    (reduce (fn [comps v]
              (if (contains? @seen v)
                comps
                (let [comp (dfs graph v)]
                  (swap! seen into comp)
                  (conj comps comp))))
            [] (range (count graph)))))
