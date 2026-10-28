(ns clojure-algorithms.project-euler.problem-120.sol1
  "Project Euler Problem 120: square remainders.

  Let r be the remainder when (a-1)^n + (a+1)^n is divided by a^2. Expanding
  the terms gives 2 if n is even and 2an if n is odd. For maximizing the
  value, 2an < a^2, so n <= (a-1)/2 with integer division. Finds the sum of
  r_max for 3 <= a <= 1000.")

(defn solution
  "Returns the sum of r_max for 3 <= a <= n."
  ([] (solution 1000))
  ([n]
   (reduce + 0 (map (fn [a] (* 2 a (quot (dec a) 2))) (range 3 (inc n))))))
