(ns clojure-algorithms.maths.digital-root
  "Port of maths/digital_root.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare digital-root)

(defn digital-root "Return the digital root of a non-negative integer number.\n\nThe digital root is the single-digit value obtained by repeatedly summing\nthe decimal digits of number until only one digit remains. The input is taken by\nits absolute value, so negative numbers behave like their positive\ncounterpart.\n\n>>> digital_root(0)\n0\n>>> digital_root(9)\n9\n>>> digital_root(38)\n2\n>>> digital_root(12345)\n6\n>>> digital_root(-45)\n9\n>>> digital_root(999999999999)\n9"
  [number] (let [number (atom number)] (try (reset! number (py-abs @number)) (while (py-truthy (py-ge @number 10)) (reset! number (reduce +' 0 (doall (for [digit (py-iter (py-str @number))] (py-int digit nil)))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @number})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))
