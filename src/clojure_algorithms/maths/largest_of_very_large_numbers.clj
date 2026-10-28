(ns clojure-algorithms.maths.largest-of-very-large-numbers
  "Port of maths/largest_of_very_large_numbers.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare res)

(defn res "Reduces large number to a more manageable number\n>>> res(5, 7)\n4.892790030352132\n>>> res(0, 5)\n0\n>>> res(3, 0)\n1\n>>> res(-1, 5)\nTraceback (most recent call last):\n...\nValueError: expected a positive input"
  [x y] (try (if (py-truthy (not (py-in 0 [x y]))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-mul y (Math/log10 x))}))) (do (if (py-truthy (py-eq x 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (do (when (py-truthy (py-eq y 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 1}))))))) (throw (AssertionError. (py-str "This should never happen"))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
