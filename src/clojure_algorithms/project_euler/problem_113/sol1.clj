(ns clojure-algorithms.project-euler.problem-113.sol1
  "Project Euler Problem 113: non-bouncy numbers.

  Working from left-to-right if no digit is exceeded by the digit to its left
  it is called an increasing number; similarly for decreasing numbers. A
  positive integer that is neither is bouncy. Counts how many numbers below a
  googol (10^100) are not bouncy.")

(defn choose
  "Calculates the binomial coefficient c(n,r) using the multiplicative
  formula."
  [n r]
  (long (Math/round (reduce * 1.0 (map (fn [i] (/ (+ n 1 (- i)) (double i)))
                                       (range 1 (inc r)))))))

(defn non-bouncy-exact
  "Calculates the number of non-bouncy numbers with exactly n digits."
  [n]
  (- (+ (choose (+ 8 n) n) (choose (+ 9 n) n)) 10))

(defn non-bouncy-upto
  "Calculates the number of non-bouncy numbers with at most n digits."
  [n]
  (reduce + 0 (map non-bouncy-exact (range 1 (inc n)))))

(defn solution
  "Calculates the number of non-bouncy numbers with at most num-digits
  digits."
  ([] (solution 100))
  ([num-digits]
   (non-bouncy-upto num-digits)))
