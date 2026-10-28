(ns clojure-algorithms.knapsack.knapsack
  "0-1 and 0-N (unbounded) knapsack via memoized recursion.
  See https://en.wikipedia.org/wiki/Knapsack_problem")

(defn knapsack
  "Returns the maximum value fitting capacity given weights and values.
  counter is the number of items under consideration; when
  allow-repetition is true items may be picked multiple times."
  [capacity weights values counter & [allow-repetition]]
  (let [memo (atom {})]
    (letfn [(rec [cap n]
              (if (or (zero? n) (zero? cap))
                0
                (if-let [cached (get @memo [cap n])]
                  cached
                  (let [result
                        (if (> (nth weights (dec n)) cap)
                          (rec cap (dec n))
                          (let [left (- cap (nth weights (dec n)))
                                included (+ (nth values (dec n))
                                            (rec left (if allow-repetition n (dec n))))
                                excluded (rec cap (dec n))]
                            (max included excluded)))]
                    (swap! memo assoc [cap n] result)
                    result))))]
      (rec capacity counter))))
