(ns clojure-algorithms.maths.special-numbers.trimorphic-number
  "A Trimorphic Number is a number whose cube ends in the same digits as the number itself.\nFor example, 4^3 = 64, which ends in 4.\n\nReference: https://en.wikipedia.org/wiki/Automorphic_number#Trimorphic_numbers"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare is-trimorphic)

(defn is-trimorphic "Checks if a number is a Trimorphic number.\n\n:param number: The number to check\n:return: True if the number is trimorphic, False otherwise\n:raises ValueError: If the input number is negative\n\n>>> is_trimorphic(0)\nTrue\n>>> is_trimorphic(1)\nTrue\n>>> is_trimorphic(4)\nTrue\n>>> is_trimorphic(5)\nTrue\n>>> is_trimorphic(24)\nTrue\n>>> is_trimorphic(249)\nTrue\n>>> is_trimorphic(2)\nFalse\n>>> is_trimorphic(7)\nFalse\n>>> is_trimorphic(10)\nFalse\n>>> is_trimorphic(-1)\nTraceback (most recent call last):\n    ...\nValueError: Input must be a non-negative integer"
  [number] (let [cube (atom nil)] (try (when (py-truthy (py-lt number 0)) (throw (ex-info (py-str "Input must be a non-negative integer") {:type :ValueError}))) (when (py-truthy (py-eq number 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value true}))) (reset! cube (py-pow number 3)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-endswith (py-str @cube) (py-str number))})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
