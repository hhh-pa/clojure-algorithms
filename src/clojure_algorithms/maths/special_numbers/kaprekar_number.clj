(ns clojure-algorithms.maths.special-numbers.kaprekar-number
  "== Kaprekar Number ==
A number n is a Kaprekar number if its square can be split into two parts
(left and right) that add up to n, where the right part is non-zero.

Examples: 1, 9, 45, 55, 99, 297, 703, 999, 2223, 4950, 5050, 7272, 9999
Reference: https://en.wikipedia.org/wiki/Kaprekar_number")

(defn- pow-int
  "Integer power: base**exp computed with exact integer arithmetic."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn is-kaprekar
  "Returns true if `number` is a Kaprekar number. Returns false for integers
below 1. Throws an ex-info with :type :TypeError when `number` is not an
integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (cond
    (< number 1) false
    (= number 1) true
    :else
    (let [square (*' number number)
          digits (count (str square))]
      (boolean
       (some (fn [split]
               (let [divisor (pow-int 10 split)
                     right (rem square divisor)
                     left (quot square divisor)]
                 (and (pos? right) (= number (+ left right)))))
             (range 1 digits))))))
