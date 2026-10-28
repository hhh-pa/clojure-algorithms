(ns clojure-algorithms.project-euler.problem-001.sol2
  "Project Euler Problem 1: multiples of 3 and 5.

  If we list all the natural numbers below 10 that are multiples of 3 or 5,
  we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.

  This solution uses the arithmetic-progression sum formula.")

(defn solution
  "Returns the sum of all the multiples of 3 or 5 below n."
  ([] (solution 1000))
  ([n]
   (let [terms3 (quot (dec n) 3)
         terms5 (quot (dec n) 5)
         terms15 (quot (dec n) 15)]
     (- (+ (quot (* terms3 (+ 6 (* (dec terms3) 3))) 2)
           (quot (* terms5 (+ 10 (* (dec terms5) 5))) 2))
        (quot (* terms15 (+ 30 (* (dec terms15) 15))) 2)))))
