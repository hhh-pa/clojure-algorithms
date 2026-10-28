(ns clojure-algorithms.financial.equated-monthly-installments)

(defn equated-monthly-installments
  "Calculate the amortization amount per month.
  A = p * r * (1 + r)^n / ((1 + r)^n - 1)
  where p is the principal, r the monthly interest rate and n the
  number of monthly payments."
  [principal rate-per-annum years-to-repay]
  (when (<= principal 0)
    (throw (ex-info "Principal borrowed must be > 0" {:principal principal})))
  (when (< rate-per-annum 0)
    (throw (ex-info "Rate of interest must be >= 0" {:rate rate-per-annum})))
  (when (or (<= years-to-repay 0) (not (integer? years-to-repay)))
    (throw (ex-info "Years to repay must be an integer > 0"
                    {:years years-to-repay})))
  (let [rate-per-month (/ rate-per-annum 12)
        n (* years-to-repay 12)]
    (/ (* principal rate-per-month (Math/pow (+ 1 rate-per-month) n))
       (- (Math/pow (+ 1 rate-per-month) n) 1))))
