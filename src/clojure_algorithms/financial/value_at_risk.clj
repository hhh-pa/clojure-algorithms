(ns clojure-algorithms.financial.value-at-risk)

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

(defn value-at-risk
  "Historical-simulation Value at Risk: the negative of the return
  quantile at (1 - confidence-level), i.e. a positive loss magnitude when
  the tail contains losses."
  ([returns] (value-at-risk returns 0.95))
  ([returns confidence-level]
   (when (empty? returns)
     (throw (ex-info "returns must not be empty" {})))
   (when (not-every? finite-number? returns)
     (throw (ex-info "returns must contain only finite numbers" {})))
   (when (not (< 0 confidence-level 1))
     (throw (ex-info "confidence_level must be strictly between 0 and 1" {})))
   (let [sorted-returns (sort returns)
         threshold (linear-interpolated-quantile sorted-returns (- 1 confidence-level))]
     (- threshold))))
