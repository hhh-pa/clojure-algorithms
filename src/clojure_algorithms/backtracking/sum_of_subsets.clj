(ns clojure-algorithms.backtracking.sum-of-subsets
  "Sum of Subsets: all subsets of non-negative integers (each used once)
  whose sum equals M. Branches that cannot reach the target are pruned.")

(defn create-state-space-tree
  "DFS from `num-index`, extending `path`. `remaining-nums-sum` bounds the
  reachable total for pruning. Returns the vector of matching subsets."
  [nums max-sum num-index path remaining-nums-sum]
  (let [path-sum (reduce + 0 path)]
    (cond
      (or (> path-sum max-sum) (< (+ remaining-nums-sum path-sum) max-sum))
      []
      (= path-sum max-sum)
      [path]
      :else
      (vec (mapcat (fn [index]
                     (create-state-space-tree nums max-sum (inc index)
                                              (conj path (nth nums index))
                                              (- remaining-nums-sum (nth nums index))))
                   (range num-index (count nums)))))))

(defn generate-sum-of-subsets-solutions
  "Returns all subsets of `nums` summing to `max-sum`."
  [nums max-sum]
  (create-state-space-tree (vec nums) max-sum 0 [] (reduce + 0 nums)))
