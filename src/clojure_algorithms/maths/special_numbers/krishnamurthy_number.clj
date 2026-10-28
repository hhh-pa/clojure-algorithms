(ns clojure-algorithms.maths.special-numbers.krishnamurthy-number
  "== Krishnamurthy Number ==
It is also known as Peterson Number
A Krishnamurthy Number is a number whose sum of the
factorial of the digits equals to the original
number itself.

For example: 145 = 1! + 4! + 5!
    So, 145 is a Krishnamurthy Number")

(defn factorial
  "Returns the factorial of `digit`."
  [digit]
  (reduce *' 1 (range 1 (inc digit))))

(defn krishnamurthy
  "Returns true if `number` is a Krishnamurthy number: the sum of the
factorials of its digits equals the number itself."
  [number]
  (loop [duplicate number, fact-sum 0]
    (if (pos? duplicate)
      (recur (quot duplicate 10)
             (+' fact-sum (factorial (rem duplicate 10))))
      (= fact-sum number))))
