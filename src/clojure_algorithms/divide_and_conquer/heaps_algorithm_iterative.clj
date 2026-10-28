(ns clojure-algorithms.divide-and-conquer.heaps-algorithm-iterative
  "Heap's iterative algorithm: all permutations of a collection.

  Each permutation is generated from the previous one by swapping a
  single pair of elements.
  Ported from TheAlgorithms/Python
  divide_and_conquer/heaps_algorithm_iterative.py.")

(defn heaps
  "Return a vector of all permutations of `coll`, each a vector."
  [coll]
  (let [arr (vec coll)
        n (count arr)]
    (if (<= n 1)
      [arr]
      (loop [a arr c (vec (repeat n 0)) i 0 res [arr]]
        (if (>= i n)
          res
          (if (< (c i) i)
            (let [a (if (even? i)
                      (assoc a 0 (a i) i (a 0))
                      (assoc a (c i) (a i) i (a (c i))))]
              (recur a (update c i inc) 0 (conj res a)))
            (recur a (assoc c i 0) (inc i) res)))))))
