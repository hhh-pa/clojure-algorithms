(ns clojure-algorithms.project-euler.problem-173.sol1
  "Project Euler Problem 173: square laminae.

  A square lamina is a square outline with a square hole possessing vertical
  and horizontal symmetry. With one hundred tiles it is possible to form
  forty-one different square laminae. Counts how many different square
  laminae can be formed using up to one million tiles.")

(defn solution
  "Returns the number of different square laminae that can be formed using
  up to limit tiles."
  ([] (solution 1000000))
  ([limit]
   (reduce + 0
           (for [outer-width (range 3 (+ (quot limit 4) 2))]
             (let [hole-low (if (> (* outer-width outer-width) limit)
                              (max (long (Math/ceil
                                          (Math/sqrt
                                           (- (* outer-width outer-width)
                                              limit))))
                                   1)
                              1)
                   hole (+ hole-low
                           (if (odd? (- outer-width hole-low)) 1 0))]
               (inc (quot (- outer-width hole 2) 2)))))))
