(ns clojure-algorithms.project-euler.problem-009.sol2
  "Project Euler Problem 9: special Pythagorean triplet.

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for
  which a^2 + b^2 = c^2. For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.

  This solution eliminates c from the two equations a^2 + b^2 = c^2 and
  a + b + c = n, then solves for b for each a.")

(defn solution
  "Returns the product of a, b, c which form a Pythagorean triplet with
  a < b < c, a^2 + b^2 = c^2 and a + b + c = n."
  ([] (solution 1000))
  ([n]
   (loop [a 1 product -1]
     (if (>= a (quot n 3))
       product
       (let [b (quot (- (* n n) (* 2 a n)) (- (* 2 n) (* 2 a)))
             c (- n a b)]
         (recur (inc a)
                (if (= (* c c) (+ (* a a) (* b b)))
                  (max product (* a b c))
                  product)))))))
