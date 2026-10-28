(ns clojure-algorithms.project-euler.problem-015.sol1
  "Project Euler Problem 15: lattice paths.

  Starting in the top left corner of a 2x2 grid, and only being able to
  move to the right and down, there are exactly 6 routes to the bottom
  right corner. How many such routes are there through a 20x20 grid?

  This solution uses the binomial coefficient C(2n, n).")

(defn- factorial
  "Returns n! as an exact integer."
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn solution
  "Returns the number of paths possible in a n x n grid starting at the
  top left corner going to the bottom right corner, moving right and
  down only."
  ([] (solution 20))
  ([n]
   (let [n2 (* 2 n)
         k (quot n2 2)]
     (quot (factorial n2) (*' (factorial k) (factorial (- n2 k)))))))
