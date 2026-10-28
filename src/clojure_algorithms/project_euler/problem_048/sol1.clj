(ns clojure-algorithms.project-euler.problem-048.sol1
  "Euler Problem 48: find the last ten digits of the series
  1^1 + 2^2 + ... + 1000^1000.")

(defn- pow
  "Returns base raised to exp as a bigint."
  [base exp]
  (loop [i 0 acc 1N]
    (if (= i exp) acc (recur (inc i) (* acc base)))))

(defn solution
  "Returns the last ten digits of 1^1 + 2^2 + ... + 1000^1000 as a string."
  []
  (let [total (reduce + 0N (map (fn [i] (pow i i)) (range 1 1001)))
        s (str total)]
    (subs s (- (count s) 10))))
