(ns clojure-algorithms.financial.present-value)

(defn present-value
  "Present value of a stream of yearly cash flows discounted at
  discount-rate. Cash flows are assumed paid at the end of the indexed year,
  starting at year 0. Result rounded to 2 decimals."
  [discount-rate cash-flows]
  (when (< discount-rate 0)
    (throw (ex-info "Discount rate cannot be negative" {})))
  (when (empty? cash-flows)
    (throw (ex-info "Cash flows list cannot be empty" {})))
  (let [total (reduce + (map-indexed (fn [i cf]
                                       (/ cf (Math/pow (+ 1 discount-rate) i)))
                                     cash-flows))]
    (/ (Math/round (* 100.0 total)) 100.0)))
