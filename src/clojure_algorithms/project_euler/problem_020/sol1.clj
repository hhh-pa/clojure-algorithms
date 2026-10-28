(ns clojure-algorithms.project-euler.problem-020.sol1
  "Project Euler Problem 20: factorial digit sum.

  n! means n x (n - 1) x ... x 3 x 2 x 1

  For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
  and the sum of the digits in the number 10! is
  3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!")

(defn factorial
  "Returns the factorial of the given number num."
  [num]
  (reduce *' 1 (range 1 (inc num))))

(defn split-and-add
  "Splits the digits of number and returns their sum."
  [number]
  (loop [number number sum-of-digits 0]
    (if (zero? number)
      sum-of-digits
      (recur (quot number 10) (+ sum-of-digits (mod number 10))))))

(defn solution
  "Returns the sum of the digits in the factorial of num."
  ([] (solution 100))
  ([num]
   (split-and-add (factorial num))))
