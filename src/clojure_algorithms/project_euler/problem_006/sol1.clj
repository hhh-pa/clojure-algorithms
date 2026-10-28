(ns clojure-algorithms.project-euler.problem-006.sol1
  "Project Euler Problem 6: sum square difference.

  The sum of the squares of the first ten natural numbers is,
      1^2 + 2^2 + ... + 10^2 = 385

  The square of the sum of the first ten natural numbers is,
      (1 + 2 + ... + 10)^2 = 55^2 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 - 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.")

(defn solution
  "Returns the difference between the sum of the squares of the first n
  natural numbers and the square of the sum."
  ([] (solution 100))
  ([n]
   (let [[sum-of-squares sum-of-ints]
         (reduce (fn [[sq s] i] [(+ sq (* i i)) (+ s i)])
                 [0 0] (range 1 (inc n)))]
     (- (* sum-of-ints sum-of-ints) sum-of-squares))))
