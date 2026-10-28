(ns clojure-algorithms.data-structures.arrays.pairwise-iteration
  "Port of data_structures/arrays/pairwise_iteration.py.
  Consecutive-pair iteration helpers.")

(defn pairwise-iteration-tee
  "Lazy seq of consecutive [a b] pairs from COLL."
  [coll]
  (map vector coll (rest coll)))

(defn pairwise-iteration-comprehension
  "Pairs of items STEP apart: items at indexes 0, step, 2*step, ...
  zipped with their successors."
  ([coll] (pairwise-iteration-comprehension coll 1))
  ([coll step]
   (let [items (vec (keep-indexed (fn [i x] (when (zero? (mod i step)) x))
                                  (vec coll)))]
     (map vector items (rest items)))))
