(ns clojure-algorithms.sorts.bead-sort
  "Bead (gravity) sort. Only works for sequences of non-negative integers.
  Ported from TheAlgorithms/Python.")

(defn bead-sort
  "Sorts a collection of non-negative integers with bead sort.
  Returns a sorted vector. Throws IllegalArgumentException if any element
  is not a non-negative integer."
  [coll]
  (let [v (vec coll)]
    (when (some #(or (not (integer? %)) (neg? %)) v)
      (throw (IllegalArgumentException.
              "Sequence must be list of non-negative integers")))
    (let [n (count v)]
      (if (zero? n)
        []
        (let [m (apply max v)
              ;; k[c] = number of beads in column c (rows with value > c)
              k (mapv (fn [c] (count (filter #(> % c) v))) (range m))]
          ;; after gravity, row i holds beads in columns with k[c] > n-1-i
          (mapv (fn [i] (count (filter #(> % (- n 1 i)) k)))
                (range n)))))))
