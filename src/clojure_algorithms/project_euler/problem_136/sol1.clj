(ns clojure-algorithms.project-euler.problem-136.sol1
  "Project Euler Problem 136: singleton difference.

  The positive integers x, y, z are consecutive terms of an arithmetic
  progression. With x = y + delta and z = y - delta, the expression
  x^2 - y^2 - z^2 rewrites as y * (4 * delta - y) = n. Counts how many
  values of n below fifty million have exactly one solution.")

(defn solution
  "Defines an n count array and loops over delta and y to get the counts,
  then returns how many values of n have a count of exactly one."
  ([] (solution 50000000))
  ([n-limit]
   (let [n-sol (long-array n-limit 0)]
     (doseq [delta (range 1 (inc (quot (inc n-limit) 4)))]
       (loop [y (dec (* 4 delta))]
         (when (> y delta)
           (let [n (* y (- (* 4 delta) y))]
             (when (< n n-limit)
               (aset n-sol n (inc (aget n-sol n)))
               (recur (dec y)))))))
     (count (filter #(= 1 %) n-sol)))))
