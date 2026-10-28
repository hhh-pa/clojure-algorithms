(ns clojure-algorithms.project-euler.problem-009.sol3
  "Project Euler Problem 9: special Pythagorean triplet.

  A Pythagorean triplet is a set of three natural numbers, a < b < c, for
  which a^2 + b^2 = c^2. For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product a*b*c.

  This solution searches a comprehension of candidate (a, b) pairs and
  takes the first one satisfying the triplet equation.")

(defn solution
  "Returns the product of a, b, c which form a Pythagorean triplet with
  a^2 + b^2 = c^2 and a + b + c = 1000."
  []
  (first (for [a (range 1 999)
               b (range a 999)
               :let [c (- 1000 a b)]
               :when (= (+ (* a a) (* b b)) (* c c))]
           (* a b c))))
