(ns clojure-algorithms.greedy-methods.best-time-to-buy-and-sell-stock
  "Maximum profit from a single buy-then-sell transaction over a price series.")

(defn max-profit
  "Returns the max profit achievable with one buy and one later sell.
  Returns 0 when no profit is possible or prices is empty."
  [prices]
  (if (empty? prices)
    0
    (:profit (reduce (fn [{:keys [min-price profit]} price]
                       {:min-price (min min-price price)
                        :profit (max profit (- price (min min-price price)))})
                     {:min-price (first prices) :profit 0}
                     prices))))
