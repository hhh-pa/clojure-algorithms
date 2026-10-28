(ns clojure-algorithms.project-euler.problem-001.sol4
  "Project Euler Problem 1: multiples of 3 and 5.

  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.

  This solution collects the multiples of 3 and of 5 below n,
  deduplicates them with a set, and sums the result.")

(defn solution
  "Returns the sum of all the multiples of 3 or 5 below n."
  ([] (solution 1000))
  ([n]
   (let [multiples-of-3 (take-while #(< % n) (map #(* 3 %) (iterate inc 1)))
         multiples-of-5 (take-while #(< % n) (map #(* 5 %) (iterate inc 1)))]
     (reduce + 0 (set (concat multiples-of-3 multiples-of-5))))))
