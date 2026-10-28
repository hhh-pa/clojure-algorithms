(ns clojure-algorithms.project-euler.problem-015.sol2
  "Project Euler Problem 15: lattice paths.

  Starting in the top left corner of a 2x2 grid, and only being able to
  move to the right and down, there are exactly 6 routes to the bottom
  right corner. How many such routes are there through a 20x20 grid?

  This solution counts the paths explicitly with dynamic programming.")

(defn solution
  "Returns the number of paths possible in a n x n grid starting at the
  top left corner going to the bottom right corner, moving right and
  down only."
  ([] (solution 20))
  ([n]
   (let [counts (atom (vec (repeat (inc n) (vec (repeat (inc n) 1)))))]
     (doseq [i (range 1 (inc n))
             j (range 1 (inc n))]
       (swap! counts assoc-in [i j]
              (+ (get-in @counts [(dec i) j])
                 (get-in @counts [i (dec j)]))))
     (get-in @counts [n n]))))
