(ns clojure-algorithms.maths.special-numbers.triangular-numbers
  "A triangular number or triangle number counts objects arranged in an\nequilateral triangle. This module provides a function to generate n'th\ntriangular number.\n\nFor more information about triangular numbers, refer to:\nhttps://en.wikipedia.org/wiki/Triangular_number"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare triangular-number)

(defn triangular-number "Generate the triangular number at the specified position.\n\nArgs:\n    position (int): The position of the triangular number to generate.\n\nReturns:\n    int: The triangular number at the specified position.\n\nRaises:\n    ValueError: If `position` is negative.\n\nExamples:\n>>> triangular_number(1)\n1\n>>> triangular_number(3)\n6\n>>> triangular_number(-1)\nTraceback (most recent call last):\n    ...\nValueError: param `position` must be non-negative"
  [position] (try (when (py-truthy (py-lt position 0)) (throw (ex-info (py-str "param `position` must be non-negative") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-floordiv (py-mul position (py-add position 1)) 2)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
