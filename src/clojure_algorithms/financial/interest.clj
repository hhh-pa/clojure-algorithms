(ns clojure-algorithms.financial.interest)

(defn simple-interest
  "Simple interest: principal * daily-interest-rate * days-between-payments."
  [principal daily-interest-rate days-between-payments]
  (when (<= days-between-payments 0)
    (throw (ex-info "days_between_payments must be > 0" {})))
  (when (< daily-interest-rate 0)
    (throw (ex-info "daily_interest_rate must be >= 0" {})))
  (when (<= principal 0)
    (throw (ex-info "principal must be > 0" {})))
  (* principal daily-interest-rate days-between-payments))

(defn compound-interest
  "Compound interest earned: principal * ((1 + rate)^periods - 1)."
  [principal nominal-annual-interest-rate-percentage number-of-compounding-periods]
  (when (<= number-of-compounding-periods 0)
    (throw (ex-info "number_of_compounding_periods must be > 0" {})))
  (when (< nominal-annual-interest-rate-percentage 0)
    (throw (ex-info "nominal_annual_interest_rate_percentage must be >= 0" {})))
  (when (<= principal 0)
    (throw (ex-info "principal must be > 0" {})))
  (* principal (- (Math/pow (+ 1 nominal-annual-interest-rate-percentage)
                            number-of-compounding-periods)
                  1)))

(defn apr-interest
  "Interest earned at an APR compounded daily over number-of-years."
  [principal nominal-annual-percentage-rate number-of-years]
  (when (<= number-of-years 0)
    (throw (ex-info "number_of_years must be > 0" {})))
  (when (< nominal-annual-percentage-rate 0)
    (throw (ex-info "nominal_annual_percentage_rate must be >= 0" {})))
  (when (<= principal 0)
    (throw (ex-info "principal must be > 0" {})))
  (compound-interest principal (/ nominal-annual-percentage-rate 365) (* number-of-years 365)))
