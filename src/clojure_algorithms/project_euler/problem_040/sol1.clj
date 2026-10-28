(ns clojure-algorithms.project-euler.problem-040.sol1
  "Euler Problem 40: Champernowne's constant is formed by concatenating the
  positive integers. If d(n) is the nth digit, find
  d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000.")

(defn solution
  "Returns the product of the 1st, 10th, 100th, ..., 1000000th digits of the
  fractional part of Champernowne's constant."
  []
  (let [sb (StringBuilder.)]
    (loop [i 1]
      (when (< (.length sb) 1000000)
        (.append sb (str i))
        (recur (inc i))))
    (let [s (.toString sb)]
      (reduce * 1 (map (fn [idx]
                         (Character/digit (.charAt s idx) 10))
                       [0 9 99 999 9999 99999 999999])))))
