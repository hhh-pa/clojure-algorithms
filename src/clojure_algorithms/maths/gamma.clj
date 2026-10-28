(ns clojure-algorithms.maths.gamma
  "Gamma function is a very useful tool in math and physics.\nIt helps calculating complex integral in a convenient way.\nfor more info: https://en.wikipedia.org/wiki/Gamma_function\nIn mathematics, the gamma function is one commonly\nused extension of the factorial function to complex numbers.\nThe gamma function is defined for all complex numbers except\nthe non-positive integers\nPython's Standard Library math.gamma() function overflows around gamma(171.624)."
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare gamma-iterative integrand gamma-recursive)

(defn gamma-iterative "Calculates the value of Gamma function of num\nwhere num is either an integer (1, 2, 3..) or a half-integer (0.5, 1.5, 2.5 ...).\n\n>>> gamma_iterative(-1)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_iterative(0)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_iterative(9)\n40320.0\n>>> from math import gamma as math_gamma\n>>> all(.99999999 < gamma_iterative(i) / math_gamma(i) <= 1.000000001\n...     for i in range(1, 50))\nTrue\n>>> gamma_iterative(-1)/math_gamma(-1) <= 1.000000001\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_iterative(3.3) - math_gamma(3.3) <= 0.00000001\nTrue"
  [& _args]
  ;; stub: original requires inf,quad; pure-Clojure port not applicable
  (throw (ex-info "requires inf,quad" {:type :UnsupportedOperation})))

(defn integrand
  [x z] (py-mul (py-pow x (py-sub z 1)) (Math/exp (py-neg x))))

(defn gamma-recursive "Calculates the value of Gamma function of num\nwhere num is either an integer (1, 2, 3..) or a half-integer (0.5, 1.5, 2.5 ...).\nImplemented using recursion\nExamples:\n>>> from math import isclose, gamma as math_gamma\n>>> gamma_recursive(0.5)\n1.7724538509055159\n>>> gamma_recursive(1)\n1.0\n>>> gamma_recursive(2)\n1.0\n>>> gamma_recursive(3.5)\n3.3233509704478426\n>>> gamma_recursive(171.5)\n9.483367566824795e+307\n>>> all(isclose(gamma_recursive(num), math_gamma(num))\n...     for num in (0.5, 2, 3.5, 171.5))\nTrue\n>>> gamma_recursive(0)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_recursive(-1.1)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_recursive(-4)\nTraceback (most recent call last):\n    ...\nValueError: math domain error\n>>> gamma_recursive(172)\nTraceback (most recent call last):\n    ...\nOverflowError: math range error\n>>> gamma_recursive(1.1)\nTraceback (most recent call last):\n    ...\nNotImplementedError: num must be an integer or a half-integer"
  [num] (try (when (py-truthy (py-le num 0)) (throw (ex-info (py-str "math domain error") {:type :ValueError}))) (when (py-truthy (py-gt num 171.5)) (throw (ex-info (py-str "math range error") {:type :OverflowError}))) (when (py-truthy (not (py-in (py-sub num (py-int num nil)) [0 0.5]))) (throw (ex-info (py-str "num must be an integer or a half-integer") {:type :NotImplementedError}))) (if (py-truthy (py-eq num 0.5)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (Math/sqrt Math/PI)}))) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-eq num 1)) 1.0 (py-mul (py-sub num 1) (gamma-recursive (py-sub num 1))))})))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

;; NOTE: gamma_iterative stubbed (inf,quad)
