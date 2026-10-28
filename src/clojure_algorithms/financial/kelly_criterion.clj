(ns clojure-algorithms.financial.kelly-criterion)

(defn kelly-criterion
  "Optimal fraction of bankroll to bet: f* = (p * b - q) / b
  where p is the win probability, q = 1 - p and b the win/loss ratio."
  [win-probability win-loss-ratio]
  (when (or (<= win-probability 0) (>= win-probability 1))
    (throw (ex-info "win_probability must be between 0 and 1 (exclusive)" {})))
  (when (<= win-loss-ratio 0)
    (throw (ex-info "win_loss_ratio must be > 0" {})))
  (let [loss-probability (- 1 win-probability)]
    (/ (- (* win-probability win-loss-ratio) loss-probability) win-loss-ratio)))

(defn kelly-criterion-extended
  "Kelly fraction from explicit win and loss amounts per unit bet."
  [win-probability win-amount loss-amount]
  (when (or (<= win-probability 0) (>= win-probability 1))
    (throw (ex-info "win_probability must be between 0 and 1 (exclusive)" {})))
  (when (<= win-amount 0)
    (throw (ex-info "win_amount must be > 0" {})))
  (when (<= loss-amount 0)
    (throw (ex-info "loss_amount must be > 0" {})))
  (kelly-criterion win-probability (/ win-amount loss-amount)))

(defn fractional-kelly
  "Fractional Kelly bet size (fraction of the full Kelly fraction).
  Defaults to half-Kelly (0.5)."
  ([win-probability win-loss-ratio] (fractional-kelly win-probability win-loss-ratio 0.5))
  ([win-probability win-loss-ratio fraction]
   (when (or (<= fraction 0) (> fraction 1))
     (throw (ex-info "fraction must be between 0 and 1 (exclusive for 0, inclusive for 1)" {})))
   (* fraction (kelly-criterion win-probability win-loss-ratio))))
