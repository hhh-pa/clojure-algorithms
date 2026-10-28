(ns clojure-algorithms.project-euler.problem-016.sol1
  "Project Euler Problem 16: power digit sum.

  2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2^1000?

  This solution sums the decimal digits of 2^power via its string form.")

(defn- pow2
  "Returns 2^power as an exact integer."
  [power]
  (loop [i 0 acc 1N]
    (if (>= i power) acc (recur (inc i) (*' acc 2)))))

(defn solution
  "Returns the sum of the digits of the number 2^power."
  ([] (solution 1000))
  ([power]
   (reduce + 0 (map #(Character/digit ^char % 10) (str (pow2 power))))))
