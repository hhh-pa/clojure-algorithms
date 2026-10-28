(ns clojure-algorithms.sorts.bubble-sort
  "Bubble sort, iterative and recursive variants.
  Ported from TheAlgorithms/Python.")

(defn bubble-sort-iterative
  "Sorts `coll` with iterative bubble sort (early exit when sorted).
  Returns a sorted vector."
  [coll]
  (let [n (count (vec coll))]
    (loop [a (vec coll) i (dec n)]
      (if (neg? i)
        a
        (let [[a2 swapped?] (loop [b a j 0 swapped? false]
                              (if (>= j i)
                                [b swapped?]
                                (if (neg? (compare (nth b (inc j)) (nth b j)))
                                  (recur (assoc b j (nth b (inc j))
                                                (inc j) (nth b j))
                                         (inc j) true)
                                  (recur b (inc j) swapped?))))]
          (if (not swapped?)
            a2
            (recur a2 (dec i))))))))

(defn- bubble-pass
  "One bubble pass over vector `v`. Returns [v swapped?]."
  [v]
  (loop [b v i 0 swapped? false]
    (if (>= i (dec (count b)))
      [b swapped?]
      (if (neg? (compare (nth b (inc i)) (nth b i)))
        (recur (assoc b i (nth b (inc i)) (inc i) (nth b i))
               (inc i) true)
        (recur b (inc i) swapped?)))))

(defn bubble-sort-recursive
  "Sorts `coll` with recursive bubble sort. Returns a sorted vector."
  [coll]
  (let [[v swapped?] (bubble-pass (vec coll))]
    (if swapped?
      (bubble-sort-recursive v)
      v)))
