(ns clojure-algorithms.project-euler.problem-055.sol1
  "Euler Problem 55: how many Lychrel numbers are there below ten-thousand? A
  number is Lychrel if it never forms a palindrome through the reverse-and-add
  process within fifty iterations.")

(defn palindrome?
  "Returns true if n is a palindrome."
  [n]
  (let [s (str n)]
    (= s (apply str (reverse s)))))

(defn sum-reverse
  "Returns the sum of n and its digit reversal."
  [n]
  (+ (bigint n) (bigint (apply str (reverse (str n))))))

(defn- lychrel?
  "Returns true if num does not reach a palindrome within fifty
  reverse-and-add iterations."
  [num]
  (loop [n num iterations 0]
    (if (>= iterations 50)
      true
      (let [n (sum-reverse n)]
        (if (palindrome? n) false (recur n (inc iterations)))))))

(defn solution
  "Returns the count of all Lychrel numbers below limit."
  ([] (solution 10000))
  ([limit]
   (count (filter lychrel? (range 1 limit)))))
