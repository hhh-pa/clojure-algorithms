(ns clojure-algorithms.maths.signum
  "Signum function -- https://en.wikipedia.org/wiki/Sign_function"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare signum test-signum)

(defn signum "Applies signum function on the number\n\nCustom test cases:\n>>> signum(-10)\n-1\n>>> signum(10)\n1\n>>> signum(0)\n0\n>>> signum(-20.5)\n-1\n>>> signum(20.5)\n1\n>>> signum(-1e-6)\n-1\n>>> signum(1e-6)\n1\n>>> signum(\"Hello\")\nTraceback (most recent call last):\n    ...\nTypeError: '<' not supported between instances of 'str' and 'int'\n>>> signum([])\nTraceback (most recent call last):\n    ...\nTypeError: '<' not supported between instances of 'list' and 'int'"
  [num] (try (when (py-truthy (py-lt num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-neg 1)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy num) 1 0)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))

(defn test-signum "Tests the signum function\n>>> test_signum()"
  [] (assert (py-truthy (py-eq (signum 5) 1))) (assert (py-truthy (py-eq (signum (py-neg 5)) (py-neg 1)))) (assert (py-truthy (py-eq (signum 0) 0))) (assert (py-truthy (py-eq (signum 10.5) 1))) (assert (py-truthy (py-eq (signum (py-neg 10.5)) (py-neg 1)))) (assert (py-truthy (py-eq (signum 1e-06) 1))) (assert (py-truthy (py-eq (signum (py-neg 1e-06)) (py-neg 1)))) (assert (py-truthy (py-eq (signum 123456789) 1))) (assert (py-truthy (py-eq (signum (py-neg 123456789)) (py-neg 1)))) nil)
