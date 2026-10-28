(ns clojure-algorithms.financial.time-and-half-pay)

(defn pay
  "Time-and-a-half pay. Hours beyond hours (default 40) earn 1.5x pay-rate."
  ([hours-worked pay-rate] (pay hours-worked pay-rate 40))
  ([hours-worked pay-rate hours]
   (assert (number? hours-worked) "Parameter 'hours_worked' must be numeric")
   (assert (number? pay-rate) "Parameter 'pay_rate' must be numeric")
   (assert (number? hours) "Parameter 'hours' must be numeric")
   (let [normal-pay (* hours-worked pay-rate)
         over-time (max 0 (- hours-worked hours))]
     (double (+ normal-pay (/ (* over-time pay-rate) 2))))))
