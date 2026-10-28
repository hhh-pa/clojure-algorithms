(ns clojure-algorithms.maths.numerical-analysis.bisection
  "Port of maths/numerical_analysis/bisection.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare bisection f)

(defn bisection "finds where function becomes 0 in [a,b] using bolzano\n>>> bisection(lambda x: x ** 3 - 1, -5, 5)\n1.0000000149011612\n>>> bisection(lambda x: x ** 3 - 1, 2, 1000)\nTraceback (most recent call last):\n    ...\nValueError: could not find root in given interval.\n>>> bisection(lambda x: x ** 2 - 4 * x + 3, 0, 2)\n1.0\n>>> bisection(lambda x: x ** 2 - 4 * x + 3, 2, 4)\n3.0\n>>> bisection(lambda x: x ** 2 - 4 * x + 3, 4, 1000)\nTraceback (most recent call last):\n    ...\nValueError: could not find root in given interval."
  [function a b] (let [end (atom nil) mid (atom nil) start (atom nil)] (try (reset! start a) (reset! end b) (if (py-truthy (py-eq (function a) 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value a}))) (do (if (py-truthy (py-eq (function b) 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value b}))) (do (if (py-truthy (py-gt (py-mul (function a) (function b)) 0)) (do (throw (ex-info (py-str "could not find root in given interval.") {:type :ValueError}))) (do (reset! mid (py-add @start (py-div (py-sub @end @start) 2.0))) (while (py-truthy (py-gt (py-abs (py-sub @start @mid)) (py-pow 10 (py-neg 7)))) (if (py-truthy (py-eq (function @mid) 0)) (do (throw (ex-info "__return__" {:__control__ true :__return__ true :value @mid}))) (do (if (py-truthy (py-lt (py-mul (function @mid) (function @start)) 0)) (do (reset! end @mid)) (do (reset! start @mid))))) (reset! mid (py-add @start (py-div (py-sub @end @start) 2.0)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @mid})))))))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn f
  [x] (py-sub (py-sub (py-pow x 3) (py-mul 2 x)) 5))
