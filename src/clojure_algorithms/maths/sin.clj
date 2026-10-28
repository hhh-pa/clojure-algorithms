(ns clojure-algorithms.maths.sin
  "Calculate sin function.\n\nIt's not a perfect function so I am rounding the result to 10 decimal places by default.\n\nFormula: sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...\nWhere: x = angle in randians.\n\nSource:\n    https://www.homeschoolmath.net/teaching/sine_calculator.php"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare sin)

(defn sin "Implement sin function.\n\n>>> sin(0.0)\n0.0\n>>> sin(90.0)\n1.0\n>>> sin(180.0)\n0.0\n>>> sin(270.0)\n-1.0\n>>> sin(0.68)\n0.0118679603\n>>> sin(1.97)\n0.0343762121\n>>> sin(64.0)\n0.8987940463\n>>> sin(9999.0)\n-0.9876883406\n>>> sin(-689.0)\n0.5150380749\n>>> sin(89.7)\n0.9999862922"
  ([angle-in-degrees] (sin angle-in-degrees 18 10))
  ([angle-in-degrees accuracy] (sin angle-in-degrees accuracy 10))
  ([angle-in-degrees accuracy rounded-values-count] (let [angle-in-degrees (atom angle-in-degrees)] (let [--v (atom nil) a (atom nil) angle-in-radians (atom nil) b (atom nil) result (atom nil)] (try (reset! angle-in-degrees (py-sub @angle-in-degrees (py-mul (py-floordiv @angle-in-degrees 360.0) 360.0))) (reset! angle-in-radians (Math/toRadians @angle-in-degrees)) (reset! result @angle-in-radians) (reset! a 3) (reset! b (py-neg 1)) (doseq [__i1 (py-iter (range accuracy))] (reset! --v __i1) (swap! result py-add (py-div (py-mul @b (py-pow @angle-in-radians @a)) (py-factorial @a))) (reset! b (py-neg @b)) (swap! a py-add 2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value (py-round @result rounded-values-count)})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))))
