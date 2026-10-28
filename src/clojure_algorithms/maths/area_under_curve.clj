(ns clojure-algorithms.maths.area-under-curve
  "Approximates the area under the curve using the trapezoidal rule"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare trapezoidal-area)

(defn trapezoidal-area "Treats curve as a collection of linear lines and sums the area of the\ntrapezium shape they form\n:param fnc: a function which defines a curve\n:param x_start: left end point to indicate the start of line segment\n:param x_end: right end point to indicate end of line segment\n:param steps: an accuracy gauge; more steps increases the accuracy\n:return: a float representing the length of the curve\n\n>>> def f(x):\n...    return 5\n>>> f\"{trapezoidal_area(f, 12.0, 14.0, 1000):.3f}\"\n'10.000'\n>>> def f(x):\n...    return 9*x**2\n>>> f\"{trapezoidal_area(f, -4.0, 0, 10000):.4f}\"\n'192.0000'\n>>> f\"{trapezoidal_area(f, -4.0, 4.0, 10000):.4f}\"\n'384.0000'"
  ([fnc x-start x-end] (trapezoidal-area fnc x-start x-end 100))
  ([fnc x-start x-end steps] (let [--v (atom nil) area (atom nil) fx1 (atom nil) fx2 (atom nil) x1 (atom nil) x2 (atom nil)] (try (reset! x1 x-start) (reset! fx1 (fnc x-start)) (reset! area 0.0) (doseq [__i1 (py-iter (range steps))] (reset! --v __i1) (reset! x2 (py-add (py-div (py-sub x-end x-start) steps) @x1)) (reset! fx2 (fnc @x2)) (swap! area py-add (py-div (py-mul (py-abs (py-add @fx2 @fx1)) (py-sub @x2 @x1)) 2)) (reset! x1 @x2) (reset! fx1 @fx2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @area})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
