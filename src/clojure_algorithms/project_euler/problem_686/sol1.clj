(ns clojure-algorithms.project-euler.problem-686.sol1
  "Project Euler Problem 686: powers of two starting with 123.

  2^7 = 128 is the first power of two whose leading digits are 12. Defines
  p(L, n) as the nth-smallest j such that 2^j begins with the digits of L.
  Uses logarithms to avoid computing huge powers: successive powers leading
  to 123 differ by 196 or 93. Finds p(123, 678910).")

(defn log-difference
  "Returns the fractional part of number * log10(2), rounded to 15 decimal
  places."
  [number]
  (let [log-number (* (/ (Math/log 2) (Math/log 10)) number)
        frac (- log-number (long log-number))]
    (/ (double (Math/round (* frac 1e15))) 1e15)))

(defn solution
  "Calculates the power of two which is the number-th smallest power of two
  whose starting digits are 123."
  ([] (solution 678910))
  ([number]
   (let [lower-limit (/ (Math/log 1.23) (Math/log 10))
         upper-limit (/ (Math/log 1.24) (Math/log 10))]
     (loop [power-iterator 90
            position 0
            previous-power 0]
       (if (>= position number)
         previous-power
         (let [difference (log-difference power-iterator)]
           (cond
             (>= difference upper-limit)
             (recur (+ power-iterator 93) position previous-power)
             (< difference lower-limit)
             (recur (+ power-iterator 196) position previous-power)
             :else
             (recur (+ power-iterator 196)
                    (inc position)
                    power-iterator))))))))
