(ns clojure-algorithms.greedy-methods.fractional-knapsack
  "Fractional knapsack via sorting by value/weight ratio and prefix sums.")

(defn- bisect-right
  "Index of the first element in sorted acc strictly greater than w."
  [acc w]
  (count (take-while #(<= % w) acc)))

(defn frac-knapsack
  "Returns the max fractional-knapsack value for values vl, weights wt,
  capacity w and item count n."
  [vl wt w n]
  (let [pairs (sort-by #(/ (first %) (second %)) > (map vector vl wt))
        vals (mapv first pairs)
        wts (mapv second pairs)
        acc (vec (reductions + wts))
        k (bisect-right acc w)]
    (cond
      (zero? k) 0
      (= k n) (reduce + (subvec vals 0 k))
      :else (+ (reduce + (subvec vals 0 k))
               (/ (* (- w (nth acc (dec k))) (nth vals k))
                  (nth wts k))))))
