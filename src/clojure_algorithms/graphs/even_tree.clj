(ns clojure-algorithms.graphs.even-tree
  "Maximum removable edges leaving all-even-sized tree components.")

(defn even-tree-cuts
  "Returns how many edges can be removed so every component has an
   even number of nodes. Tree is rooted at 1."
  [tree]
  (let [cuts (atom 0)]
    (letfn [(dfs [u parent]
              (let [size (reduce + 1 (map #(dfs % u)
                                          (remove #(= % parent)
                                                  (get tree u []))))]
                (when (and (even? size) (not= u 1))
                  (swap! cuts inc))
                size))]
      (dfs 1 nil))
    @cuts))
