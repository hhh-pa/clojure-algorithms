(ns clojure-algorithms.maths.special-numbers.disarum-number
  "A number n is said to be a Disarium number if the sum of its digits raised to
their respective positions equals the number itself.

Examples of Disarium Numbers: 1, 2, 3, 4, 5, 6, 7, 8, 9, 89, 135, 175, 518, 598, ...

https://rosettacode.org/wiki/Disarium_numbers")

(defn- pow-int
  "Integer power: base**exp computed with exact integer arithmetic."
  [base exp]
  (reduce *' 1 (repeat exp base)))

(defn is-disarium-number
  "Returns true if `number` is a Disarium number: the sum of its digits raised
to their (1-based) positions equals the number itself. Returns false for
integers below 1. Throws an ex-info with :type :TypeError when `number` is
not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (if (< number 1)
    false
    (let [total (reduce +' 0
                        (map-indexed (fn [idx ch]
                                       (pow-int (Character/digit ^char ch 10)
                                                (inc idx)))
                                     (str number)))]
      (= total number))))
