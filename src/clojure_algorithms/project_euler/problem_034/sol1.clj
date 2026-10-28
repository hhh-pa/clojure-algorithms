(ns clojure-algorithms.project-euler.problem-034.sol1
  "Euler Problem 34: find the sum of all numbers which are equal to the sum of
  the factorial of their digits (1 and 2 excluded since they are not sums).")

(defn- factorial
  "Returns n!."
  [n]
  (reduce * 1N (range 1 (inc n))))

(def digit-factorial
  "Map from digit character to its factorial."
  (into {} (map (fn [d] [(first (str d)) (factorial d)]) (range 10))))

(defn sum-of-digit-factorial
  "Returns the sum of the factorials of the digits in n."
  [n]
  (reduce + (map digit-factorial (str n))))

(defn solution
  "Returns the sum of all numbers equal to the sum of the factorials of their
  digits."
  []
  (let [limit (inc (* 7 (factorial 9)))]
    (reduce + (filter (fn [i] (= (sum-of-digit-factorial i) i))
                      (range 3 limit)))))
