(ns clojure-algorithms.maths.special-numbers.polygonal-numbers
  "Port of maths/special_numbers/polygonal_numbers.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare polygonal-num)

(defn polygonal-num "Returns the `num`th `sides`-gonal number. It is assumed that `num` >= 0 and\n`sides` >= 3 (see for reference https://en.wikipedia.org/wiki/Polygonal_number).\n\n>>> polygonal_num(0, 3)\n0\n>>> polygonal_num(3, 3)\n6\n>>> polygonal_num(5, 4)\n25\n>>> polygonal_num(2, 5)\n5\n>>> polygonal_num(-1, 0)\nTraceback (most recent call last):\n    ...\nValueError: Invalid input: num must be >= 0 and sides must be >= 3.\n>>> polygonal_num(0, 2)\nTraceback (most recent call last):\n    ...\nValueError: Invalid input: num must be >= 0 and sides must be >= 3."
  [num sides] (try (when (py-truthy (let [__b1 (py-lt num 0)] (if (py-truthy __b1) __b1 (py-lt sides 3)))) (throw (ex-info (py-str "Invalid input: num must be >= 0 and sides must be >= 3.") {:type :ValueError}))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-floordiv (py-sub (py-mul (py-sub sides 2) (py-pow num 2)) (py-mul (py-sub sides 4) num)) 2)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))
