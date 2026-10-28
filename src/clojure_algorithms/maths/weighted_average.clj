(ns clojure-algorithms.maths.weighted-average
  "Port of maths/weighted_average.py")

(defn weighted-average
  "Returns the weighted average of `values` given their corresponding
  `weights`. See https://en.wikipedia.org/wiki/Weighted_arithmetic_mean"
  [values weights]
  (when (empty? values)
    (throw (ex-info "Inputs cannot be empty" {:type :ValueError})))
  (when (not= (count values) (count weights))
    (throw (ex-info "Values and weights must have the same length" {:type :ValueError})))
  (let [total (reduce + weights)]
    (when (zero? total)
      (throw (ex-info "Sum of weights cannot be zero" {:type :ValueError})))
    (/ (double (reduce + (map * values weights))) total)))
