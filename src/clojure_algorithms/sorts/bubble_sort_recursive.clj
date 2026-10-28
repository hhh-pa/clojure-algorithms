(ns clojure-algorithms.sorts.bubble-sort-recursive
  "Recursive bubble sort as a standalone namespace.
  Ported from TheAlgorithms/Python bubble_sort_recursive.py.")

(defn bubble-sort-recursive
  "Sorts `coll` with recursive bubble sort: one pass bubbles the largest
  element to the end, then the prefix is sorted recursively.
  Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (if (<= n 1)
      v
      (let [[b swapped?] (loop [a v i 0 swapped? false]
                           (if (>= i (dec n))
                             [a swapped?]
                             (if (neg? (compare (nth a (inc i)) (nth a i)))
                               (recur (assoc a i (nth a (inc i))
                                             (inc i) (nth a i))
                                      (inc i) true)
                               (recur a (inc i) swapped?))))]
        (if (not swapped?)
          b
          (conj (bubble-sort-recursive (pop b)) (peek b)))))))
