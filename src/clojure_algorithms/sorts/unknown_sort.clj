(ns clojure-algorithms.sorts.unknown-sort
  "Min/max removal sort (named `merge_sort` in the Python original).
  Ported from TheAlgorithms/Python.")

(defn merge-sort
  "Sorts `coll` by repeatedly removing the min and max. Returns a vector."
  [coll]
  (loop [rest-v (vec coll) start [] end []]
    (if (<= (count rest-v) 1)
      (into (into start rest-v) (rseq (vec end)))
      (let [lo (reduce (fn [m x] (if (neg? (compare x m)) x m)) rest-v)
            hi (reduce (fn [m x] (if (pos? (compare x m)) x m)) rest-v)
            drop-one (fn [v x]
                       (let [i (.indexOf ^java.util.List v x)]
                         (into (subvec v 0 i) (subvec v (inc i)))))]
        (recur (drop-one (drop-one rest-v lo) hi)
               (conj start lo) (conj end hi))))))
