(ns clojure-algorithms.maths.numerical-analysis.proper-fractions
  "Port of maths/numerical_analysis/proper_fractions.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare proper-fractions)

(defn proper-fractions "this algorithm returns a list of proper fractions, in the\nrange between 0 and 1, which can be formed with the given denominator\nhttps://en.wikipedia.org/wiki/Fraction#Proper_and_improper_fractions\n\n>>> proper_fractions(10)\n['1/10', '3/10', '7/10', '9/10']\n>>> proper_fractions(5)\n['1/5', '2/5', '3/5', '4/5']\n>>> proper_fractions(-15)\nTraceback (most recent call last):\n    ...\nValueError: The Denominator Cannot be less than 0\n>>> proper_fractions(0)\n[]\n>>> proper_fractions(1.2)\nTraceback (most recent call last):\n    ...\nValueError: The Denominator must be an integer"
  [denominator] (try (when (py-truthy (py-lt denominator 0)) (throw (ex-info (py-str "The Denominator Cannot be less than 0") {:type :ValueError}))) (when (py-truthy (number? denominator)) (throw (ex-info (py-str "The Denominator must be an integer") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (vec (for [numerator (py-iter (range 1 denominator)) :when (py-truthy (py-eq (py-gcd numerator denominator) 1))] (str (py-str numerator) "/" (py-str denominator))))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
