(ns clojure-algorithms.financial.fractional-differentiation)

(defn calculate-weights
  "Calculate the weights for fractional differentiation.
  w_0 = 1, w_k = -w_{k-1} * (d - k + 1) / k."
  [degree length]
  (loop [k 1 weights [1.0]]
    (if (>= k length)
      weights
      (recur (inc k) (conj weights (* -1 (peek weights) (/ (+ (- degree k) 1) k)))))))

(defn fracdiff-fixedwindow
  "Fractional differentiation of price-series with a fixed window size.
  The first window-size entries are NaN (warm-up period)."
  [price-series degree window-size]
  (when (> window-size (count price-series))
    (throw (ex-info "window_size cannot exceed the length of price_series" {})))
  (let [weights (calculate-weights degree window-size)
        warmup (vec (repeat window-size Double/NaN))
        values (for [i (range window-size (count price-series))]
                 (reduce + (map-indexed (fn [j w]
                                          (* w (nth price-series (- i j))))
                                        weights)))]
    (into warmup (map double values))))
