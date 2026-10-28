(ns clojure-algorithms.maths.special-numbers.spy-number
  "== Spy Number ==
 spy number is defined as a positive integer where the
 sum of its digits equals the product of its digits.

Examples of Spy Numbers: 22,123,1124,...
https://www.scribd.com/document/895653665/Interesting-Number-Programs")

(defn is-spy-number
  "Returns true if `number` is a spy number: the sum of its digits equals the
product of its digits. Returns false for integers below 1. Throws an ex-info
with :type :TypeError when `number` is not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (if (<= number 0)
    false
    (let [digits (map #(Character/digit ^char % 10) (str number))]
      (= (reduce + 0 digits) (reduce * 1 digits)))))
