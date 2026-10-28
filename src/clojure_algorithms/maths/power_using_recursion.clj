(ns clojure-algorithms.maths.power-using-recursion
  "== Raise base to the power of exponent using recursion ==\n    Input -->\n        Enter the base: 3\n        Enter the exponent: 4\n    Output  -->\n        3 to the power of 4 is 81\n    Input -->\n        Enter the base: 2\n        Enter the exponent: 0\n    Output -->\n        2 to the power of 0 is 1"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare power)

(defn power "Calculate the power of a base raised to an exponent.\n\n>>> power(3, 4)\n81\n>>> power(2, 0)\n1\n>>> all(power(base, exponent) == pow(base, exponent)\n...     for base in range(-10, 10) for exponent in range(10))\nTrue\n>>> power('a', 1)\n'a'\n>>> power('a', 2)\nTraceback (most recent call last):\n    ...\nTypeError: can't multiply sequence by non-int of type 'str'\n>>> power('a', 'b')\nTraceback (most recent call last):\n    ...\nTypeError: unsupported operand type(s) for -: 'str' and 'int'\n>>> power(2, -1)\nTraceback (most recent call last):\n    ...\nRecursionError: maximum recursion depth exceeded\n>>> power(0, 0)\n1\n>>> power(0, 1)\n0\n>>> power(5,6)\n15625\n>>> power(23, 12)\n21914624432020321"
  [base exponent] (if (py-truthy exponent) (py-mul base (power base (py-sub exponent 1))) 1))
