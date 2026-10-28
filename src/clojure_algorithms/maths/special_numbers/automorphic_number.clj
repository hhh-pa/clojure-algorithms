(ns clojure-algorithms.maths.special-numbers.automorphic-number
  "== Automorphic Numbers ==
A number n is said to be a Automorphic number if
the square of n \"ends\" in the same digits as n itself.

Examples of Automorphic Numbers: 0, 1, 5, 6, 25, 76, 376, 625, 9376, 90625, ...
https://en.wikipedia.org/wiki/Automorphic_number"
  (:require [clojure.string :as str]))

(defn is-automorphic-number
  "Returns true if `number` is automorphic, i.e. its square ends in the same
digits as the number itself. Returns false for negative integers.
Throws an ex-info with :type :TypeError when `number` is not an integer."
  [number]
  (when-not (integer? number)
    (throw (ex-info (str "Input value of [number=" number "] must be an integer")
                    {:type :TypeError})))
  (if (neg? number)
    false
    (str/ends-with? (str (*' number number)) (str number))))
