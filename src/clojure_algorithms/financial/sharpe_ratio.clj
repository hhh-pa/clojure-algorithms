(ns clojure-algorithms.financial.sharpe-ratio)

(defn sharpe-ratio
  "Sharpe Ratio: (mean(returns) - risk-free-rate) / sample-std-dev.
  Returns +Inf for zero volatility with positive excess return, 0.0 when
  both are zero."
  ([returns] (sharpe-ratio returns 0.0))
  ([returns risk-free-rate]
   (when (empty? returns)
     (throw (ex-info "returns list must not be empty" {})))
   (when (< (count returns) 2)
     (throw (ex-info "returns list must contain at least 2 values" {})))
   (let [mean (/ (reduce + returns) (count returns))
         excess (- mean risk-free-rate)
         variance (/ (reduce + (map #(Math/pow (- % mean) 2) returns))
                     (dec (count returns)))
         std-dev (Math/sqrt variance)]
     (if (zero? std-dev)
       (if (pos? excess) Double/POSITIVE_INFINITY 0.0)
       (/ excess std-dev)))))

(defn annualized-sharpe-ratio
  "Annualized Sharpe Ratio: periodic Sharpe * sqrt(periods-per-year).
  periods-per-year is 252 (daily), 52, 12 or 4 by convention."
  ([returns] (annualized-sharpe-ratio returns 0.0 252))
  ([returns risk-free-rate periods-per-year]
   (when (<= periods-per-year 0)
     (throw (ex-info "periods_per_year must be > 0" {})))
   (let [periodic (sharpe-ratio returns risk-free-rate)]
     (if (Double/isInfinite periodic)
       Double/POSITIVE_INFINITY
       (* periodic (Math/sqrt periods-per-year))))))
