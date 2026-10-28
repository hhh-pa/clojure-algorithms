(ns clojure-algorithms.project-euler.problem-056.sol1
  "Euler Problem 56: considering natural numbers of the form a^b where
  a, b < 100, what is the maximum digital sum?")

(defn- pow
  "Returns base raised to exp as a bigint."
  [base exp]
  (loop [i 0 acc 1N]
    (if (= i exp) acc (recur (inc i) (* acc base)))))

(defn- digital-sum
  "Returns the sum of the decimal digits of n."
  [n]
  (reduce + (map (fn [ch] (Character/digit ch 10)) (str n))))

(defn solution
  "Returns the maximum digital sum of base^power for bases below a and powers
  below b."
  ([] (solution 100 100))
  ([a] (solution a 100))
  ([a b]
   (apply max 0 (for [base (range a) power (range b)]
                  (digital-sum (pow base power))))))
