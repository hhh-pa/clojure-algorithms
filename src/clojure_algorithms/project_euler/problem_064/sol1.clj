(ns clojure-algorithms.project-euler.problem-064.sol1
  "Project Euler 64: odd period square roots.

  All square roots are periodic when written as continued fractions.
  Exactly four continued fractions, for N <= 13, have an odd period.
  How many continued fractions for N <= 10000 have an odd period?")

(defn continuous-fraction-period
  "Returns the continued fraction period of a number n."
  [n]
  (let [root (int (Math/sqrt n))]
    (loop [numerator 0.0
           denominator 1.0
           integer-part root
           period 0]
      (if (= integer-part (* 2 root))
        period
        (let [numerator (- (* denominator integer-part) numerator)
              denominator (/ (- n (* numerator numerator)) denominator)
              integer-part (int (/ (+ root numerator) denominator))]
          (recur numerator denominator integer-part (inc period)))))))

(defn solution
  "Returns the count of numbers <= n with odd continued fraction periods.
  Calls continuous-fraction-period for numbers which are not perfect
  squares."
  ([] (solution 10000))
  ([n]
   (count (filter (fn [i]
                    (let [sr (Math/sqrt i)]
                      (and (not (== sr (Math/floor sr)))
                           (odd? (continuous-fraction-period i)))))
                  (range 2 (inc n))))))
