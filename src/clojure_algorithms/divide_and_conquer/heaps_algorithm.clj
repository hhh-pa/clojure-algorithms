(ns clojure-algorithms.divide-and-conquer.heaps-algorithm
  "Heap's recursive algorithm: all permutations of a collection.

  Each permutation is generated from the previous one by swapping a
  single pair of elements.
  Ported from TheAlgorithms/Python divide_and_conquer/heaps_algorithm.py.")

(defn- generate
  "Heap's recursion on the first `k` slots of `a`.
  Returns [permutations final-array], threading the array like the
  Python original's in-place mutation."
  [k a res]
  (if (= k 1)
    [(conj res a) a]
    (let [[res arr] (generate (dec k) a res)]
      (loop [i 0 cur-a arr cur-res res]
        (if (>= i (dec k))
          [cur-res cur-a]
          (let [swapped (if (even? k)
                          (assoc cur-a i (cur-a (dec k)) (dec k) (cur-a i))
                          (assoc cur-a 0 (cur-a (dec k)) (dec k) (cur-a 0)))
                [res2 arr2] (generate (dec k) swapped cur-res)]
            (recur (inc i) arr2 res2)))))))

(defn heaps
  "Return a vector of all permutations of `coll`, each a vector."
  [coll]
  (let [arr (vec coll)]
    (if (<= (count arr) 1)
      [arr]
      (first (generate (count arr) arr [])))))
