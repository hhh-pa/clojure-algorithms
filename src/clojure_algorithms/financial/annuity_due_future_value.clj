(ns clojure-algorithms.financial.annuity-due-future-value)

(defn annuity-due-future-value
  "Calculate the future value of an annuity due (payments made at the
  beginning of each term).

  term-payment        payment made at the beginning of each term
  number-of-payments  the number of payments
  term-interest-rate  the interest rate for each term
  Returns the future (maturity) value of the annuity due."
  [term-payment number-of-payments term-interest-rate]
  (let [annuity-factor (/ (- (Math/pow (+ 1 term-interest-rate) number-of-payments) 1)
                          term-interest-rate)]
    (* term-payment annuity-factor (+ 1 term-interest-rate))))
