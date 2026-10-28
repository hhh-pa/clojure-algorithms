(ns clojure-algorithms.project-euler.problem-001.sol3
  "Project Euler Problem 1: multiples of 3 and 5.

  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.

  This solution walks the series using the repeating step pattern
  3, 2, 1, 3, 1, 2, 3.")

(defn solution
  "Returns the sum of all the multiples of 3 or 5 below n."
  ([] (solution 1000))
  ([n]
   (loop [num 0 total 0 steps (cycle [3 2 1 3 1 2 3])]
     (let [num (+ num (first steps))]
       (if (>= num n)
         total
         (recur num (+ total num) (rest steps)))))))
