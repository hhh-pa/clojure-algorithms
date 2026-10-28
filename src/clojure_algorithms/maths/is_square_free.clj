(ns clojure-algorithms.maths.is-square-free
  "References: wikipedia:square free number\npsf/black : True\nruff : True"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare is-square-free)

(defn is-square-free "# doctest: +NORMALIZE_WHITESPACE\nThis functions takes a list of prime factors as input.\nreturns True if the factors are square free.\n>>> is_square_free([1, 1, 2, 3, 4])\nFalse\n\nThese are wrong but should return some value\nit simply checks for repetition in the numbers.\n>>> is_square_free([1, 3, 4, 'sd', 0.0])\nTrue\n\n>>> is_square_free([1, 0.5, 2, 0.0])\nTrue\n>>> is_square_free([1, 2, 2, 5])\nFalse\n>>> is_square_free('asd')\nTrue\n>>> is_square_free(24)\nTraceback (most recent call last):\n    ...\nTypeError: 'int' object is not iterable"
  [factors] (py-eq (count (py-set factors)) (count factors)))
