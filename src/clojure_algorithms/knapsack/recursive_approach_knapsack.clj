(ns clojure-algorithms.knapsack.recursive-approach-knapsack
  "Naive recursive 0-1 knapsack (exponential; mirrors the Python original).")

(defn knapsack
  "Returns the maximum gain for weights, values, number-of-items,
  max-weight, starting at index."
  [weights values number-of-items max-weight index]
  (if (= index number-of-items)
    0
    (let [skipped (knapsack weights values number-of-items max-weight (inc index))]
      (if (<= (nth weights index) max-weight)
        (max skipped
             (+ (nth values index)
                (knapsack weights values number-of-items
                          (- max-weight (nth weights index)) (inc index))))
        skipped))))
