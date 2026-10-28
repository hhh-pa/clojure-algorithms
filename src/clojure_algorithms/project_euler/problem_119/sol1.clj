(ns clojure-algorithms.project-euler.problem-119.sol1
  "Project Euler Problem 119: digit power sum.

  The number 512 is interesting because it is equal to the sum of its digits
  raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Defines a(n) to be the
  nth term of this sequence where a2 = 512 and a10 = 614656. Finds a30.")

(defn digit-sum
  "Returns the sum of the digits of the number."
  [n]
  (reduce + 0 (map #(Character/digit ^char % 10) (str n))))

(defn solution
  "Returns the value of the nth digit power sum."
  ([] (solution 30))
  ([n]
   (let [digit-powers (sort (for [digit (range 2 100)
                                  power (range 2 100)
                                  :let [number (.toBigInteger
                                                (BigDecimal.
                                                 ^double (Math/pow digit power)))]
                                  :when (= digit (digit-sum number))]
                              number))]
     (nth digit-powers (dec n)))))
