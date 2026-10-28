(ns clojure-algorithms.project-euler.problem-190.sol1
  "Project Euler Problem 190: maximising a weighted product.

  Let S_m be the m-tuple of positive real numbers with x_1 + ... + x_m = m
  for which P_m = x_1 * x_2^2 * ... * x_m^m is maximised. Partial
  derivatives show x_i = i * x_1 with x_1 = 2 / (m + 1). Finds the sum of
  the integer parts of P_m for m from 2 to 15.")

(defn solution
  "Calculates the sum of the integer parts of P_m for m from 2 to n."
  ([] (solution 15))
  ([n]
   (reduce + 0
           (for [m (range 2 (inc n))]
             (let [x1 (/ 2.0 (inc m))]
               (long (reduce * 1.0
                             (for [i (range 1 (inc m))]
                               (Math/pow (* i x1) i)))))))))
