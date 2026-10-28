(ns clojure-algorithms.graphs.greedy-min-vertex-cover
  "Greedy 2-approximation for minimum vertex cover.")

(defn- first-max-by
  "Element with maximal (k %), keeping the first on ties."
  [k coll]
  (reduce (fn [a b] (if (> (k b) (k a)) b a)) coll))

(defn greedy-min-vertex-cover
  "Repeatedly takes the highest-degree vertex until all edges are covered."
  [graph]
  (loop [adj (into {} (map (fn [[k v]] [k (vec v)]) graph))
         chosen #{}]
    (let [active (filter #(seq (val %)) adj)]
      (if (empty? active)
        chosen
        (let [v (first-max-by #(count (get adj %)) (sort (keys adj)))
              rest-adj (dissoc adj v)]
          (recur (into {} (map (fn [[k vs]]
                                 [k (vec (remove #(= % v) vs))])
                               rest-adj))
                 (conj chosen v)))))))
