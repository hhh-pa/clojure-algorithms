(ns clojure-algorithms.sorts.odd-even-sort
  "Odd-even sort. Ported from TheAlgorithms/Python.")

(defn- pass-swap
  "Compare-swaps disjoint adjacent pairs of `v` starting at `start`."
  [v start]
  (loop [a v i start swapped? false]
    (if (>= i (dec (count a)))
      [a swapped?]
      (if (pos? (compare (nth a i) (nth a (inc i))))
        (recur (assoc a i (nth a (inc i)) (inc i) (nth a i))
               (+ i 2) true)
        (recur a (+ i 2) swapped?)))))

(defn odd-even-sort
  "Sorts `coll` with odd-even sort. Returns a sorted vector."
  [coll]
  (loop [a (vec coll)]
    (let [[a2 s1?] (pass-swap a 0)
          [a3 s2?] (pass-swap a2 1)]
      (if (or s1? s2?) (recur a3) a3))))
