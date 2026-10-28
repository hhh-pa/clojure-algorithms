(ns clojure-algorithms.sorts.recursive-mergesort-array
  "Merge sort over arrays via recursive splitting.
  Ported from TheAlgorithms/Python."
  (:refer-clojure :exclude [merge]))

(defn merge
  "Returns a new vector with `coll` sorted ascending via merge sort."
  [coll]
  (let [v (vec coll)]
    (if (<= (count v) 1)
      v
      (let [mid (quot (count v) 2)
            left (merge (subvec v 0 mid))
            right (merge (subvec v mid))]
        (loop [l (seq left) r (seq right) acc (transient [])]
          (cond
            (nil? l) (persistent! (reduce conj! acc r))
            (nil? r) (persistent! (reduce conj! acc l))
            (neg? (compare (first l) (first r)))
            (recur (next l) r (conj! acc (first l)))
            :else
            (recur l (next r) (conj! acc (first r)))))))))
