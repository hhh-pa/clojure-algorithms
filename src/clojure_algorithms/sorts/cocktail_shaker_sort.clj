(ns clojure-algorithms.sorts.cocktail-shaker-sort
  "Cocktail shaker sort (bidirectional bubble sort).
  Ported from TheAlgorithms/Python.")

(defn cocktail-shaker-sort
  "Sorts `coll` with cocktail shaker sort. Returns a sorted vector."
  [coll]
  (let [n (count (vec coll))]
    (loop [a (vec coll) start 0 end (dec n)]
      (if (>= start end)
        a
        (let [[a2 swapped?]
              (loop [b a i start sw false]
                (if (>= i end)
                  [b sw]
                  (if (pos? (compare (nth b i) (nth b (inc i))))
                    (recur (assoc b i (nth b (inc i)) (inc i) (nth b i))
                           (inc i) true)
                    (recur b (inc i) sw))))]
          (if (not swapped?)
            a2
            (let [end2 (dec end)
                  [a3 swapped2?]
                  (loop [b a2 i end2 sw false]
                    (if (<= i start)
                      [b sw]
                      (if (neg? (compare (nth b i) (nth b (dec i))))
                        (recur (assoc b i (nth b (dec i)) (dec i) (nth b i))
                               (dec i) true)
                        (recur b (dec i) sw))))]
              (if (not swapped2?)
                a3
                (recur a3 (inc start) end2)))))))))
