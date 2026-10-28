(ns clojure-algorithms.project-euler.problem-001.sol7
  "Project Euler Problem 1: multiples of 3 and 5.

  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.")

(defn solution
  "Returns the sum of all the multiples of 3 or 5 below n."
  ([] (solution 1000))
  ([n]
   (reduce (fn [result i]
             (if (or (zero? (mod i 3)) (zero? (mod i 5)))
               (+ result i)
               result))
           0 (range n))))
