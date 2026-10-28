(ns clojure-algorithms.greedy-methods.fractional-knapsack-2
  "Fractional knapsack returning both max value and per-item fractions.
  See https://en.wikipedia.org/wiki/Continuous_knapsack_problem")

(defn fractional-knapsack
  "Greedily fills capacity with items in descending value/weight ratio.
  Returns [max-value fractions] where fractions[i] is in [0,1]."
  [value weight capacity]
  (let [index (sort-by #(/ (nth value %) (nth weight %)) > (range (count value)))]
    (loop [remaining capacity
           max-value 0
           fractions (vec (repeat (count value) 0))
           [i & rest-idx] index]
      (if (nil? i)
        [max-value fractions]
        (let [w (nth weight i)
              v (nth value i)]
          (if (<= w remaining)
            (recur (- remaining w)
                   (+ max-value v)
                   (assoc fractions i 1)
                   rest-idx)
            (let [frac (/ remaining w)]
              [(+ max-value (* v frac))
               (assoc fractions i frac)])))))))
