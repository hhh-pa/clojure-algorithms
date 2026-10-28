(ns clojure-algorithms.project-euler.problem-137.sol1
  "Project Euler Problem 137: Fibonacci golden nuggets.

  The polynomial sequence rewrites in finite form as
  A_F(x) = x / (1 - x - x^2). Solving for rational x that give A_F(x) as a
  positive integer shows the nth golden nugget is F(2n) * F(2n+1), where
  F(k) is the kth Fibonacci number.")

(defn solution
  "Calculates Fibonacci numbers 2n and 2n+1, and returns their product."
  ([] (solution 15))
  ([n]
   (let [k (* 2 n)]
     (loop [i (dec k)
            fib1 1
            fib2 1]
       (if (zero? i)
         (* fib1 fib2)
         (recur (dec i) fib2 (+ fib1 fib2)))))))
