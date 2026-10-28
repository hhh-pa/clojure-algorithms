(ns clojure-algorithms.project-euler.problem-086.sol1
  "Project Euler 86: cuboid route.

  Label the 3 side-lengths of the cuboid a, b, c with 1 <= a <= b <= c.
  The shortest distance between 2 opposite corners is sqrt((a+b)^2 +
  c^2), an integer iff (a+b), c form the first 2 sides of a Pythagorean
  triplet. Find the least value of M such that the number of solutions
  first exceeds one million.")

(defn solution
  "Returns the least value of M such that there are more than limit
  cuboids of side lengths 1 <= a,b,c <= M for which the shortest route
  between two opposite vertices is integral."
  ([] (solution 1000000))
  ([limit]
   (loop [max-cuboid-size 0 num-cuboids 0]
     (if (> num-cuboids limit)
       max-cuboid-size
       (let [m (inc max-cuboid-size)
             add (reduce +
                         (for [d (range 2 (inc (* 2 m)))
                               :let [r (Math/sqrt (+ (* d d) (* m m)))]
                               :when (== r (Math/floor r))]
                           (+ (- (min m (quot d 2))
                                 (max 1 (- d m)))
                              1)))]
         (recur m (+ num-cuboids add)))))))
