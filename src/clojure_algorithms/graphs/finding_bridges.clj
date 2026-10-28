(ns clojure-algorithms.graphs.finding-bridges
  "Bridges (critical edges) of an undirected graph via DFS lowlinks.")

(defn compute-bridges
  "Returns the bridges as [a b] pairs with a <= b."
  [graph]
  (let [n (count graph)
        low (atom (vec (repeat n 0)))
        visited (atom (vec (repeat n false)))
        bridges (atom [])]
    (letfn [(dfs [at parent id_]
              (swap! visited assoc at true)
              (swap! low assoc at id_)
              (let [id_ (inc id_)]
                (doseq [to (get graph at [])]
                  (cond
                    (= to parent) nil
                    (not (nth @visited to))
                    (do (dfs to at id_)
                        (swap! low assoc at
                               (min (nth @low at) (nth @low to)))
                        (when (<= id_ (nth @low to))
                          (swap! bridges conj
                                 (if (< at to) [at to] [to at]))))
                    :else (swap! low assoc at
                                 (min (nth @low at) (nth @low to)))))))]
      (doseq [i (range n)]
        (when-not (nth @visited i)
          (dfs i -1 0))))
    @bridges))
