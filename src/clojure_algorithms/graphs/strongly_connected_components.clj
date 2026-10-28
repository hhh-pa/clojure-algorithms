(ns clojure-algorithms.graphs.strongly-connected-components
  "Strongly connected components via finishing-order + reversed DFS.")

(defn topology-sort
  "Returns vertices of vert's reachable subgraph in finishing order."
  [graph vert]
  (let [seen (atom #{})]
    (letfn [(visit [v]
              (when-not (contains? @seen v)
                (swap! seen conj v)
                (vec (concat (mapcat visit (get graph v [])) [v]))))]
      (vec (visit vert)))))

(defn find-components
  "Returns vert's component in the reversed graph."
  [reversed-graph vert]
  (let [seen (atom #{})]
    (letfn [(visit [v]
              (when-not (contains? @seen v)
                (swap! seen conj v)
                (into [v] (mapcat visit (get reversed-graph v [])))))]
      (vec (visit vert)))))

(defn strongly-connected-components
  "Returns the strongly connected components."
  [graph]
  (let [n (count graph)
        rev (reduce (fn [r [u vs]]
                      (reduce (fn [r v] (update r v conj u)) r vs))
                    (into {} (map #(vector % []) (range n))) graph)
        seen (atom #{})
        order (atom [])]
    (doseq [i (range n)]
      (when-not (contains? @seen i)
        (letfn [(visit [v]
                  (when-not (contains? @seen v)
                    (swap! seen conj v)
                    (doseq [w (get graph v [])] (visit w))
                    (swap! order conj v)))]
          (visit i))))
    (reset! seen #{})
    (reduce (fn [comps v]
              (if (contains? @seen v)
                comps
                (let [comp (atom [])]
                  (letfn [(visit [u]
                            (when-not (contains? @seen u)
                              (swap! seen conj u)
                              (swap! comp conj u)
                              (doseq [w (get rev u [])] (visit w))))]
                    (visit v))
                  (conj comps @comp))))
            [] (reverse @order))))
