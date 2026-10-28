(ns clojure-algorithms.project-euler.problem-129.sol1
  "Project Euler Problem 129: repunit divisibility.

  A number consisting entirely of ones is called a repunit. R(k) is a repunit
  of length k. Given n with GCD(n, 10) = 1, A(n) is the least k such that
  R(k) is divisible by n. Finds the least value of n for which A(n) first
  exceeds one million.")

(defn least-divisible-repunit
  "Returns the least value k such that the repunit of length k is divisible
  by divisor."
  [divisor]
  (if (or (zero? (mod divisor 5)) (zero? (mod divisor 2)))
    0
    (loop [repunit 1
           repunit-index 1]
      (if (zero? repunit)
        repunit-index
        (recur (mod (+ (* 10 repunit) 1) divisor)
               (inc repunit-index))))))

(defn solution
  "Returns the least value of n for which least-divisible-repunit first
  exceeds limit."
  ([] (solution 1000000))
  ([limit]
   (let [start (let [d (dec limit)] (if (even? d) (inc d) d))]
     (loop [divisor start]
       (if (> (least-divisible-repunit divisor) limit)
         divisor
         (recur (+ divisor 2)))))))
