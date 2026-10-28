(ns clojure-algorithms.sorts.merge-sort
  "Classic top-down merge sort. Ported from TheAlgorithms/Python.")

(defn- merge-halves
  [left right]
  (loop [l (seq left) r (seq right) acc (transient [])]
    (cond
      (nil? l) (persistent! (reduce conj! acc r))
      (nil? r) (persistent! (reduce conj! acc l))
      (neg? (compare (first r) (first l)))
      (recur l (next r) (conj! acc (first r)))
      :else
      (recur (next l) r (conj! acc (first l))))))

(defn merge-sort
  "Sorts `coll` with merge sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [mid (quot (count v) 2)]
        (merge-halves (merge-sort (subvec v 0 mid))
                      (merge-sort (subvec v mid)))))))
