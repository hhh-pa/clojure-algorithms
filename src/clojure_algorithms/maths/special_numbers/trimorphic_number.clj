(ns clojure-algorithms.maths.special-numbers.trimorphic-number
  "A Trimorphic Number is a number whose cube ends in the same digits as the number itself.
For example, 4^3 = 64, which ends in 4.

Reference: https://en.wikipedia.org/wiki/Automorphic_number#Trimorphic_numbers"
  (:require [clojure.string :as str]))

(defn is-trimorphic
  "Returns true if `number` is trimorphic: its cube ends in the same digits as
the number itself. Throws an ex-info with :type :ValueError when `number` is
negative."
  [number]
  (when (neg? number)
    (throw (ex-info "Input must be a non-negative integer" {:type :ValueError})))
  (if (zero? number)
    true
    (str/ends-with? (str (*' number number number)) (str number))))
