(ns clojure-algorithms.sorts.iterative-merge-sort
  "Bottom-up (iterative) merge sort. Ported from TheAlgorithms/Python.")

(defn merge-ranges
  "Merges the sorted runs [low, mid) and [mid, high) of vector `v`.
  Returns the updated vector."
  [v low mid high]
  (let [left (subvec v low mid)
        right (subvec v mid high)
        merged (loop [l (seq left) r (seq right) acc (transient [])]
                 (cond
                   (nil? l) (persistent! (reduce conj! acc r))
                   (nil? r) (persistent! (reduce conj! acc l))
                   (pos? (compare (first l) (first r)))
                   (recur l (next r) (conj! acc (first r)))
                   :else
                   (recur (next l) r (conj! acc (first l)))))]
    (into (into (subvec v 0 low) merged) (subvec v high))))

(defn iter-merge-sort
  "Sorts `coll` with bottom-up merge sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (<= n 1)
      v
      (loop [a v width 1]
        (if (>= width n)
          a
          (recur (loop [b a i 0]
                   (if (>= i n)
                     b
                     (recur (merge-ranges b i (min (+ i width) n)
                                          (min (+ i (* 2 width)) n))
                            (+ i (* 2 width)))))
                 (* 2 width)))))))
