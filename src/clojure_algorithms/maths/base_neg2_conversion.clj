(ns clojure-algorithms.maths.base-neg2-conversion
  "Port of maths/base_neg2_conversion.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str]))

(declare decimal-to-negative-base-2)

(defn decimal-to-negative-base-2 "This function returns the number negative base 2\n    of the decimal number of the input data.\n\nArgs:\n    int: The decimal number to convert.\n\nReturns:\n    int: The negative base 2 number.\n\nExamples:\n    >>> decimal_to_negative_base_2(0)\n    0\n    >>> decimal_to_negative_base_2(-19)\n    111101\n    >>> decimal_to_negative_base_2(4)\n    100\n    >>> decimal_to_negative_base_2(7)\n    11011"
  [num] (let [num (atom num)] (let [ans (atom nil) rem-v (atom nil)] (try (when (py-truthy (py-eq @num 0)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value 0}))) (reset! ans "") (while (py-truthy (not (py-eq @num 0))) (let [__d1 (vec [(py-floordiv @num (py-neg 2)) (py-mod @num (py-neg 2))])] (reset! num (nth __d1 0)) (reset! rem-v (nth __d1 1))) (when (py-truthy (py-lt @rem-v 0)) (swap! rem-v py-add 2) (swap! num py-add 1)) (reset! ans (py-add (py-str @rem-v) @ans))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-int @ans nil)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
