(ns clojure-algorithms.maths.special-numbers.armstrong-numbers
  "An Armstrong number is equal to the sum of its own digits each raised to the
power of the number of digits.

For example, 370 is an Armstrong number because 3*3*3 + 7*7*7 + 0*0*0 = 370.

Armstrong numbers are also called Narcissistic numbers and Pluperfect numbers.

On-Line Encyclopedia of Integer Sequences entry: https://oeis.org/A005188"
  (:require [clojure.string :as str]))

(def PASSING
  [1 153 370 371 1634 24678051 115132219018763992565095597973971522401])

(def FAILING [-153 -1 0 1.2 200 "A" [] {} nil])

(defn- pow-int
  "Integer power: base**exp computed with exact integer arithmetic."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn- digits-of
  "Decimal digits of a non-negative integer n as a sequence of ints."
  [n]
  (map #(Character/digit ^char % 10) (str n)))

(defn armstrong-number
  "Returns true if n is an Armstrong number, i.e. it equals the sum of its own
digits each raised to the power of the number of digits. Returns false for
non-integers and integers below 1."
  [n]
  (if (or (not (integer? n)) (< n 1))
    false
    (let [digits (digits-of n)
          total (reduce +' 0 (map #(pow-int % (count digits)) digits))]
      (= n total))))

(defn pluperfect-number
  "Returns true if n is a pluperfect number (same as an Armstrong number),
computed via a histogram of its digits. Returns false for non-integers and
integers below 1."
  [n]
  (if (or (not (integer? n)) (< n 1))
    false
    (let [s (str n)
          digit-total (count s)
          total (reduce +' 0
                        (map (fn [[ch cnt]]
                               (*' cnt (pow-int (Character/digit ^char ch 10)
                                                digit-total)))
                             (frequencies s)))]
      (= n total))))

(defn narcissistic-number
  "Returns true if n is a narcissistic number (same as an Armstrong number).
Returns false for non-integers and integers below 1."
  [n]
  (if (or (not (integer? n)) (< n 1))
    false
    (let [s (str n)
          expo (count s)]
      (= n (reduce +' 0 (map #(pow-int (Character/digit ^char % 10) expo) s))))))

(defn main
  "Request that user input an integer and tell them if it is Armstrong number."
  []
  (print "Enter an integer to see if it is an Armstrong number: ")
  (flush)
  (let [num (Integer/parseInt (str/trim (read-line)))]
    (doseq [check [armstrong-number narcissistic-number pluperfect-number]]
      (println (str num " is " (if (check num) "" "not ")
                    "an Armstrong number.")))))
