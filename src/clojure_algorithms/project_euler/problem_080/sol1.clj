(ns clojure-algorithms.project-euler.problem-080.sol1
  "Project Euler 80: square root digital expansion.

  For the first one hundred natural numbers, find the total of the
  digital sums of the first one hundred decimal digits for all the
  irrational square roots. Uses BigDecimal square roots with extra
  precision so rounding cannot corrupt the first hundred digits."
  (:import [java.math BigDecimal MathContext RoundingMode]))

(defn solution
  "Returns the total of the digital sums of the first one hundred
  decimal digits for all irrational square roots of 2..99."
  []
  (let [context (MathContext. 110 RoundingMode/HALF_UP)]
    (reduce +
            (for [i (range 2 100)]
              (let [s (.toPlainString (.sqrt (BigDecimal. (str i)) context))]
                (if (> (count s) 1)
                  (+ (Character/digit (first s) 10)
                     (reduce + (map #(Character/digit ^char % 10)
                                    (subs s 2 101))))
                  0))))))
