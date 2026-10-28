(ns clojure-algorithms.project-euler.problem-001.sol6
  "Project Euler Problem 1: multiples of 3 and 5.

  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.")

(defn solution
  "Returns the sum of all the multiples of 3 or 5 below n."
  ([] (solution 1000))
  ([n]
   (loop [a 3 result 0]
     (if (>= a n)
       result
       (recur (inc a)
              (cond (or (zero? (mod a 3)) (zero? (mod a 5))) (+ result a)
                    (zero? (mod a 15)) (- result a)
                    :else result))))))
