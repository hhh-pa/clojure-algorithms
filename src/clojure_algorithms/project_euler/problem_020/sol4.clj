(ns clojure-algorithms.project-euler.problem-020.sol4
  "Project Euler Problem 20: factorial digit sum.

  n! means n x (n - 1) x ... x 3 x 2 x 1

  For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
  and the sum of the digits in the number 10! is
  3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!

  This solution builds the factorial in a loop, then adds up its
  decimal digits.")

(defn solution
  "Returns the sum of the digits in the factorial of num."
  ([] (solution 100))
  ([num]
   (let [fact (loop [i 1 fact 1]
                (if (> i num) fact (recur (inc i) (*' fact i))))]
     (reduce + 0 (map #(Character/digit ^char % 10) (str fact))))))
