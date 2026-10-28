(ns clojure-algorithms.graphs.articulation-points
  "Articulation points of an undirected graph (Tarjan's algorithm).")

(defn compute-ap
  "Returns the sorted articulation points of the graph."
  [graph]
  (let [n (count graph)
        disc (atom (vec (repeat n -1)))
        low (atom (vec (repeat n -1)))
        timer (atom 0)
        art (atom #{})]
    (letfn [(dfs [at parent]
              (swap! timer inc)
              (let [t @timer]
                (swap! disc assoc at t)
                (swap! low assoc at t))
              (let [children (atom 0)]
                (doseq [to (get graph at [])]
                  (cond
                    (= to parent) nil
                    (= -1 (nth @disc to))
                    (do (swap! children inc)
                        (dfs to at)
                        (swap! low assoc at
                               (min (nth @low at) (nth @low to)))
                        (when (and (not= parent -1)
                                   (>= (nth @low to) (nth @disc at)))
                          (swap! art conj at)))
                    :else (swap! low assoc at
                                 (min (nth @low at) (nth @disc to)))))
                (when (and (= parent -1) (> @children 1))
                  (swap! art conj at))))]
      (doseq [i (range n)]
        (when (= -1 (nth @disc i))
          (dfs i -1))))
    (vec (sort @art))))
