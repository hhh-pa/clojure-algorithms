(ns clojure-algorithms.sorts.pancake-sort
  "Pancake sort: repeatedly flip the max of the unsorted prefix into place.
  Ported from TheAlgorithms/Python.")

(defn pancake-sort
  "Sorts `coll` with pancake sort. Returns a sorted vector."
  [coll]
  (loop [a (vec coll) cur (count (vec coll))]
    (if (<= cur 1)
      a
      (let [;; index of first max in prefix (compare-based for generality)
            max-idx (reduce (fn [mi i]
                              (if (pos? (compare (nth a i) (nth a mi))) i mi))
                            0 (range cur))
            flip (fn [v k] (into (vec (rseq (subvec v 0 k))) (subvec v k)))]
        (recur (-> a (flip (inc max-idx)) (flip cur)) (dec cur))))))
