(ns clojure-algorithms.project-euler.problem-020.sol2
  "Project Euler Problem 20: factorial digit sum.

  n! means n x (n - 1) x ... x 3 x 2 x 1

  For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
  and the sum of the digits in the number 10! is
  3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!

  This solution sums the decimal digits of num! via its string form.")

(defn solution
  "Returns the sum of the digits in the factorial of num."
  ([] (solution 100))
  ([num]
   (reduce + 0 (map #(Character/digit ^char % 10)
                    (str (reduce *' 1 (range 1 (inc num))))))))
