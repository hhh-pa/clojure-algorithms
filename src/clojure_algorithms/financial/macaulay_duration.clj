(ns clojure-algorithms.financial.macaulay-duration)

(defn macaulay-duration
  "Macaulay Duration of a bond in years: the present-value-weighted
  average time to receive the bond's cash flows."
  [face-value coupon-rate periods yield-rate]
  (when (<= face-value 0)
    (throw (ex-info "face_value must be > 0" {})))
  (when (< coupon-rate 0)
    (throw (ex-info "coupon_rate must be >= 0" {})))
  (when (< periods 1)
    (throw (ex-info "periods must be > 0" {})))
  (when (<= yield-rate 0)
    (throw (ex-info "yield_rate must be > 0" {})))
  (let [{:keys [pv weighted]}
        (reduce (fn [{:keys [pv weighted]} period]
                  (let [cash-flow (+ (* face-value coupon-rate)
                                     (if (= period periods) face-value 0))
                        discount (Math/pow (+ 1 yield-rate) period)]
                    {:pv (+ pv (/ cash-flow discount))
                     :weighted (+ weighted (/ (* period cash-flow) discount))}))
                {:pv 0.0 :weighted 0.0}
                (range 1 (inc periods)))]
    (/ weighted pv)))
