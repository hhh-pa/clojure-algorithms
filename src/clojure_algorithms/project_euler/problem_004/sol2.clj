(ns clojure-algorithms.project-euler.problem-004.sol2
  "Project Euler Problem 4: largest palindrome product.

  A palindromic number reads the same both ways. The largest palindrome made
  from the product of two 2-digit numbers is 9009 = 91 x 99.

  Find the largest palindrome made from the product of two 3-digit numbers.

  This solution multiplies every pair of 3-digit numbers and keeps the
  largest palindromic product below n.")

(defn solution
  "Returns the largest palindrome made from the product of two 3-digit
  numbers which is less than n."
  ([] (solution 998001))
  ([n]
   (reduce max 0
           (for [i (range 999 99 -1)
                 j (range 999 99 -1)
                 :let [p (* i j)
                       s (str p)]
                 :when (and (= s (apply str (reverse s))) (< p n))]
             p))))
