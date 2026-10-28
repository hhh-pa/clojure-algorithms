(ns clojure-algorithms.maths.special-numbers.krishnamurthy-number
  "== Krishnamurthy Number ==\nIt is also known as Peterson Number\nA Krishnamurthy Number is a number whose sum of the\nfactorial of the digits equals to the original\nnumber itself.\n\nFor example: 145 = 1! + 4! + 5!\n    So, 145 is a Krishnamurthy Number"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare factorial krishnamurthy)

(defn factorial ">>> factorial(3)\n6\n>>> factorial(0)\n1\n>>> factorial(5)\n120"
  [digit] (if (py-truthy (py-in digit [0 1])) 1 (py-mul digit (factorial (py-sub digit 1)))))

(defn krishnamurthy ">>> krishnamurthy(145)\nTrue\n>>> krishnamurthy(240)\nFalse\n>>> krishnamurthy(1)\nTrue"
  [number] (let [digit (atom nil) duplicate (atom nil) fact-sum (atom nil)] (try (reset! fact-sum 0) (reset! duplicate number) (while (py-truthy (py-gt @duplicate 0)) (let [__d1 (vec [(py-floordiv @duplicate 10) (py-mod @duplicate 10)])] (reset! duplicate (nth __d1 0)) (reset! digit (nth __d1 1))) (swap! fact-sum py-add (factorial @digit))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-eq @fact-sum number)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
