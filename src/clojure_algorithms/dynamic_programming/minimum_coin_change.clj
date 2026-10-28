(ns clojure-algorithms.dynamic-programming.minimum-coin-change
  "Number of ways to make change for `n` with coin denominations `s`.
  Ported from TheAlgorithms/Python dynamic_programming/minimum_coin_change.py.")

(defn dp-count
  "Ways to make amount `n` from coins `s`."
  [s n]
  (if (neg? n)
    0
    (let [table (atom (assoc (vec (repeat (inc n) 0)) 0 1))]
      (doseq [coin s
              j (range coin (inc n))]
        (swap! table update j + (@table (- j coin))))
      (@table n))))
