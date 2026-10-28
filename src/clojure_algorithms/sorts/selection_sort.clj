(ns clojure-algorithms.sorts.selection-sort
  "Selection sort. Ported from TheAlgorithms/Python.")

(defn selection-sort
  "Sorts `coll` with selection sort. Returns a sorted vector."
  [coll]
  (let [v (vec coll)
        n (count v)]
    (loop [a v i 0]
      (if (>= i (dec n))
        a
        (let [mi (reduce (fn [m k]
                           (if (neg? (compare (nth a k) (nth a m))) k m))
                         i (range (inc i) n))]
          (recur (if (= mi i) a (assoc a i (nth a mi) mi (nth a i)))
                 (inc i)))))))
