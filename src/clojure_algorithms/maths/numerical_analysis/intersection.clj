(ns clojure-algorithms.maths.numerical-analysis.intersection
  "Port of maths/numerical_analysis/intersection.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare intersection f)

(defn intersection "function is the f we want to find its root\nx0 and x1 are two random starting points\n>>> intersection(lambda x: x ** 3 - 1, -5, 5)\n0.9999999999954654\n>>> intersection(lambda x: x ** 3 - 1, 5, 5)\nTraceback (most recent call last):\n    ...\nZeroDivisionError: float division by zero, could not find root\n>>> intersection(lambda x: x ** 3 - 1, 100, 200)\n1.0000000000003888\n>>> intersection(lambda x: x ** 2 - 4 * x + 3, 0, 2)\n0.9999999998088019\n>>> intersection(lambda x: x ** 2 - 4 * x + 3, 2, 4)\n2.9999999998088023\n>>> intersection(lambda x: x ** 2 - 4 * x + 3, 4, 1000)\n3.0000000001786042\n>>> intersection(math.sin, -math.pi, math.pi)\n0.0\n>>> intersection(math.cos, -math.pi, math.pi)\nTraceback (most recent call last):\n    ...\nZeroDivisionError: float division by zero, could not find root"
  [function x0 x1] (let [x-n (atom nil) x-n1 (atom nil) x-n2 (atom nil)] (try (reset! x-n x0) (reset! x-n1 x1) (while (py-truthy true) (when (py-truthy (let [__b1 (py-eq @x-n @x-n1)] (if (py-truthy __b1) __b1 (py-eq (function @x-n1) (function @x-n))))) (throw (ex-info (py-str "float division by zero, could not find root") {:type :ZeroDivisionError}))) (reset! x-n2 (py-sub @x-n1 (py-div (function @x-n1) (py-div (py-sub (function @x-n1) (function @x-n)) (py-sub @x-n1 @x-n))))) (when (py-truthy (py-lt (py-abs (py-sub @x-n2 @x-n1)) (py-pow 10 (py-neg 5)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @x-n2}))) (reset! x-n @x-n1) (reset! x-n1 @x-n2)) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn f "function is f(x) = x^3 - 2x - 5\n>>> f(2)\n-1.0"
  [x] (py-sub (py-sub (py-pow x 3) (py-mul 2 x)) 5))
