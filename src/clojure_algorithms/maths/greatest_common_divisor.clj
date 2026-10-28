(ns clojure-algorithms.maths.greatest-common-divisor
  "Greatest Common Divisor.

  Wikipedia reference: https://en.wikipedia.org/wiki/Greatest_common_divisor

  gcd(a, b) = gcd(a, -b) = gcd(-a, b) = gcd(-a, -b) by definition of divisibility"
  (:require [clojure.string :as str]))

(defn greatest-common-divisor
  "Calculate Greatest Common Divisor (GCD)."
  [a b]
  (if (zero? a)
    (abs b)
    (recur (mod b a) a)))

(defn gcd-by-iterative
  "Calculate GCD iteratively (no additional stack frames)."
  [x y]
  (loop [x x
         y y]
    (if (zero? y)
      (abs x)
      (recur y (mod x y)))))

(defn main
  "Call Greatest Common Divisor function."
  []
  (try
    (let [nums (str/split (do (print "Enter two integers separated by comma (,): ")
                              (flush)
                              (read-line))
                          #",")
          num-1 (Long/parseLong (nth nums 0))
          num-2 (Long/parseLong (nth nums 1))]
      (println (str "greatest_common_divisor(" num-1 ", " num-2 ") = "
                    (greatest-common-divisor num-1 num-2)))
      (println (str "By iterative gcd(" num-1 ", " num-2 ") = "
                    (gcd-by-iterative num-1 num-2))))
    (catch Exception _
      (println "Wrong input")))
  nil)
