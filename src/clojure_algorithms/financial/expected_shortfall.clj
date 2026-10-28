(ns clojure-algorithms.financial.expected-shortfall)

(defn- linear-interpolated-quantile
  "Linear interpolation between the closest ranks (NumPy default, type 7)."
  [sorted-values quantile]
  (let [position (* (dec (count sorted-values)) quantile)
        lower-index (int position)
        fraction (- position lower-index)]
    (if (= lower-index (dec (count sorted-values)))
      (last sorted-values)
      (+ (* (nth sorted-values lower-index) (- 1 fraction))
         (* (nth sorted-values (inc lower-index)) fraction)))))

(defn- finite-number? [v]
  (and (number? v) (Double/isFinite (double v))))

(defn expected-shortfall
  "Historical-simulation Expected Shortfall (CVaR) of a portfolio.
  The tail contains every observed return at or below the VaR threshold;
  the result is the negative of that tail's average."
  ([returns] (expected-shortfall returns 0.95))
  ([returns confidence-level]
   (when (empty? returns)
     (throw (ex-info "returns must not be empty" {})))
   (when (not-every? finite-number? returns)
     (throw (ex-info "returns must contain only finite numbers" {})))
   (when (not (< 0 confidence-level 1))
     (throw (ex-info "confidence_level must be strictly between 0 and 1" {})))
   (let [sorted-returns (sort returns)
         threshold (linear-interpolated-quantile sorted-returns (- 1 confidence-level))
         tail (filter #(<= % threshold) sorted-returns)]
     (/ (double (- (reduce + tail))) (count tail)))))
