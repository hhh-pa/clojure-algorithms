(ns clojure-algorithms.financial.ordinary-annuity-future-value)

(defn ordinary-annuity-future-value
  "Calculate the future value of an ordinary annuity (payments made at
  the end of each term).

  term-payment        payment made at the end of each term
  number-of-payments  the number of payments
  term-interest-rate  the interest rate for each term"
  [term-payment number-of-payments term-interest-rate]
  (let [annuity-factor (/ (- (Math/pow (+ 1 term-interest-rate) number-of-payments) 1)
                          term-interest-rate)]
    (* term-payment annuity-factor)))
