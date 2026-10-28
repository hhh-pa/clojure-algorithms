(ns clojure-algorithms.project-euler.problem-053.sol1
  "Euler Problem 53: how many values of nCr for 1 <= n <= 100 exceed
  one million?")

(defn- factorial
  "Returns n!."
  [n]
  (reduce * 1N (range 1 (inc n))))

(defn combinations
  "Returns the number of ways to select r items from n (nCr)."
  [n r]
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defn solution
  "Returns the count of nCr values for 1 <= n <= 100 greater than one million."
  []
  (count (for [i (range 1 101)
               j (range 1 (inc i))
               :when (> (combinations i j) 1000000)]
           1)))
