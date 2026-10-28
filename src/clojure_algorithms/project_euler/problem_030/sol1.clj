(ns clojure-algorithms.project-euler.problem-030.sol1
  "Euler Problem 30: find the sum of all numbers that can be written as the sum
  of the fifth powers of their digits.")

(def digits-fifth-power
  "Map from digit character to its fifth power."
  (into {} (map (fn [d] [(first (str d)) (long (Math/pow d 5))]) (range 10))))

(defn digits-fifth-powers-sum
  "Returns the sum of the fifth powers of the digits of number."
  [number]
  (reduce + (map digits-fifth-power (str number))))

(defn solution
  "Returns the sum of all numbers between 1000 and 1000000 equal to the sum of
  the fifth powers of their digits."
  []
  (reduce + (filter (fn [n] (= n (digits-fifth-powers-sum n)))
                    (range 1000 1000000))))
