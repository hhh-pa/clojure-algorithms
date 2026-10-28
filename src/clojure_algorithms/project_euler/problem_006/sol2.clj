(ns clojure-algorithms.project-euler.problem-006.sol2
  "Project Euler Problem 6: sum square difference.

  The sum of the squares of the first ten natural numbers is,
      1^2 + 2^2 + ... + 10^2 = 385

  The square of the sum of the first ten natural numbers is,
      (1 + 2 + ... + 10)^2 = 55^2 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 - 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.

  This solution uses closed-form formulas with integer arithmetic.")

(defn solution
  "Returns the difference between the sum of the squares of the first n
  natural numbers and the square of the sum."
  ([] (solution 100))
  ([n]
   (let [sum-cubes (let [t (quot (* n (inc n)) 2)] (* t t))
         sum-squares (quot (* n (inc n) (inc (* 2 n))) 6)]
     (- sum-cubes sum-squares))))
