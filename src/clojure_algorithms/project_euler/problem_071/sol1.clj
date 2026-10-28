(ns clojure-algorithms.project-euler.problem-071.sol1
  "Project Euler 71: ordered fractions.

  By listing the set of reduced proper fractions for d <= 1,000,000 in
  ascending order of size, find the numerator of the fraction immediately
  to the left of 3/7.")

(defn solution
  "Returns the closest numerator of the fraction immediately to the left
  of the given fraction (numerator/denominator) from a list of reduced
  proper fractions with denominators up to limit."
  ([] (solution 3 7 1000000))
  ([numerator] (solution numerator 7 1000000))
  ([numerator denominator] (solution numerator denominator 1000000))
  ([numerator denominator limit]
   (loop [current-denominator 1 max-numerator 0 max-denominator 1]
     (if (> current-denominator limit)
       max-numerator
       (let [current-numerator (quot (* current-denominator numerator)
                                     denominator)
             current-numerator (if (zero? (mod current-denominator
                                               denominator))
                                 (dec current-numerator)
                                 current-numerator)]
         (if (> (* current-numerator max-denominator)
                (* current-denominator max-numerator))
           (recur (inc current-denominator)
                  current-numerator current-denominator)
           (recur (inc current-denominator)
                  max-numerator max-denominator)))))))
