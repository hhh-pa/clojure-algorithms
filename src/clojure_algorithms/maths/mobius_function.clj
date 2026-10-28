(ns clojure-algorithms.maths.mobius-function
  "References: https://en.wikipedia.org/wiki/M%C3%B6bius_function\nReferences: wikipedia:square free number\npsf/black : True\nruff : True"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure-algorithms.maths.is-square-free :as m2] [clojure-algorithms.maths.prime-factors :as m1]))

(declare mobius)

(defn mobius "Mobius function\n>>> mobius(24)\n0\n>>> mobius(-1)\n1\n>>> mobius('asd')\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'str'\n>>> mobius(10**400)\n0\n>>> mobius(10**-400)\n1\n>>> mobius(-1424)\n1\n>>> mobius([1, '2', 2.0])\nTraceback (most recent call last):\n    ...\nTypeError: '<=' not supported between instances of 'int' and 'list'"
  [n] (let [factors (atom nil)] (try (reset! factors (m1/prime-factors n)) (when (py-truthy (m2/is-square-free @factors)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (if (py-truthy (py-mod (count @factors) 2)) (py-neg 1) 1)}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
