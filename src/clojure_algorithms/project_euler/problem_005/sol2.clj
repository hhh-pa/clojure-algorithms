(ns clojure-algorithms.project-euler.problem-005.sol2
  "Project Euler Problem 5: smallest multiple.

  2520 is the smallest number that can be divided by each of the numbers
  from 1 to 10 without any remainder.

  What is the smallest positive number that is evenly divisible by all
  of the numbers from 1 to 20?

  This solution folds the least common multiple over 1..n, using the
  property lcm(a, b) * gcd(a, b) = a * b.")

(defn- gcd
  "Returns the greatest common divisor of x and y via the Euclidean
  algorithm."
  [x y]
  (loop [x x y y]
    (if (zero? y) x (recur y (mod x y)))))

(defn lcm
  "Returns the least common multiple of x and y."
  [x y]
  (quot (* x y) (gcd x y)))

(defn solution
  "Returns the smallest positive number that is evenly divisible (divisible
  with no remainder) by all of the numbers from 1 to n."
  ([] (solution 20))
  ([n]
   (reduce lcm 1 (range 1 (inc n)))))
