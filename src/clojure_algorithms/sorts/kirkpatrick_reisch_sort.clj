(ns clojure-algorithms.sorts.kirkpatrick-reisch-sort
  "Kirkpatrick-Reisch sort: split into sqrt(n) blocks, sort each block,
  then k-way merge. Ported from TheAlgorithms/Python.")

(defn kirkpatrick-reisch-sort
  "Sorts `coll` with Kirkpatrick-Reisch sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (<= n 1)
      v
      (let [block-size (max 1 (int (Math/sqrt n)))
            blocks (mapv vec (map sort (partition-all block-size v)))]
        (loop [heads (mapv seq blocks) acc (transient [])]
          (let [live (keep-indexed (fn [i s] (when s [i (first s)])) heads)]
            (if (empty? live)
              (persistent! acc)
              (let [[mi mv] (reduce (fn [best [i x]]
                                      (if (neg? (compare x (second best)))
                                        [i x] best))
                                    (first live) (rest live))]
                (recur (assoc heads mi (next (nth heads mi)))
                       (conj! acc mv))))))))))
