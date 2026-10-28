(ns clojure-algorithms.knapsack.greedy-knapsack
  "Greedy fractional knapsack: take items whole in descending
  profit-per-weight order, fractionally for the last item that fits.")

(defn calc-profit
  "Returns the maximum gain for profits, weights and max-weight.
  Throws when inputs are invalid (length mismatch, non-positive
  max-weight, negative profit or weight)."
  [profit weight max-weight]
  (when (not= (count profit) (count weight))
    (throw (ex-info "The length of profit and weight must be same." {})))
  (when (<= max-weight 0)
    (throw (ex-info "max_weight must greater than zero." {})))
  (when (some neg? profit)
    (throw (ex-info "Profit can not be negative." {})))
  (when (some neg? weight)
    (throw (ex-info "Weight can not be negative." {})))
  (let [order (sort-by #(/ (double (nth profit %)) (nth weight %)) >
                       (range (count profit)))]
    (loop [limit 0
           gain 0
           [i & rest-order] order]
      (if (nil? i)
        gain
        (let [w (nth weight i)
              p (nth profit i)]
          (if (>= (- max-weight limit) w)
            (recur (+ limit w) (+ gain p) rest-order)
            (+ gain (* (/ (- max-weight limit) (double w)) p))))))))
