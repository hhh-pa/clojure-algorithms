(ns clojure-algorithms.financial.straight-line-depreciation)

(defn straight-line-depreciation
  "Straight-line depreciation schedule: spreads (purchase-value -
  residual-value, default 0.0) evenly over useful-years.
  Returns a vector of annual depreciation expenses; the final year absorbs
  any rounding remainder."
  ([useful-years purchase-value]
   (straight-line-depreciation useful-years purchase-value 0.0))
  ([useful-years purchase-value residual-value]
   (when (not (integer? useful-years))
     (throw (ex-info "Useful years must be an integer" {})))
   (when (< useful-years 1)
     (throw (ex-info "Useful years cannot be less than 1" {})))
   (when (not (number? purchase-value))
     (throw (ex-info "Purchase value must be numeric" {})))
   (when (not (number? residual-value))
     (throw (ex-info "Residual value must be numeric" {})))
   (when (< purchase-value 0.0)
     (throw (ex-info "Purchase value cannot be less than zero" {})))
   (when (< purchase-value residual-value)
     (throw (ex-info "Purchase value cannot be less than residual value" {})))
   (let [depreciable-cost (- (double purchase-value) (double residual-value))
         annual (/ depreciable-cost useful-years)]
     (loop [period 0 accumulated 0.0 result []]
       (if (= period (dec useful-years))
         (conj result (- depreciable-cost accumulated))
         (recur (inc period) (+ accumulated annual) (conj result annual)))))))
