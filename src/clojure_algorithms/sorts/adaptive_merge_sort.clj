(ns clojure-algorithms.sorts.adaptive-merge-sort
  "Adaptive merge sort.

  A merge sort that skips the merge step when the two sorted halves are
  already in order (last element of the left half <= first element of
  the right half), giving O(n) behaviour on nearly-sorted input.
  Ported from TheAlgorithms/Python.")

(defn- merge-halves
  "Stable merge of two sorted vectors."
  [left right]
  (loop [l (seq left) r (seq right) acc (transient [])]
    (cond
      (nil? l) (persistent! (reduce conj! acc r))
      (nil? r) (persistent! (reduce conj! acc l))
      (neg? (compare (first r) (first l)))
      (recur l (next r) (conj! acc (first r)))
      :else
      (recur (next l) r (conj! acc (first l))))))

(defn adaptive-merge-sort
  "Sorts `coll` with adaptive merge sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)]
    (if (< (count v) 2)
      v
      (let [mid (quot (count v) 2)
            left (adaptive-merge-sort (subvec v 0 mid))
            right (adaptive-merge-sort (subvec v mid))]
        (if (not (neg? (compare (first right) (peek left))))
          (into left right)
          (merge-halves left right))))))
