(ns clojure-algorithms.maths.line-length
  "Port of maths/line_length.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.set :as set]))

(declare line-length)

(defn line-length "Approximates the arc length of a line segment by treating the curve as a\nsequence of linear lines and summing their lengths\n:param fnc: a function which defines a curve\n:param x_start: left end point to indicate the start of line segment\n:param x_end: right end point to indicate end of line segment\n:param steps: an accuracy gauge; more steps increases accuracy\n:return: a float representing the length of the curve\n\n>>> def f(x):\n...    return x\n>>> f\"{line_length(f, 0, 1, 10):.6f}\"\n'1.414214'\n\n>>> def f(x):\n...    return 1\n>>> f\"{line_length(f, -5.5, 4.5):.6f}\"\n'10.000000'\n\n>>> def f(x):\n...    return math.sin(5 * x) + math.cos(10 * x) + x * x/10\n>>> f\"{line_length(f, 0.0, 10.0, 10000):.6f}\"\n'69.534930'"
  ([fnc x-start x-end] (line-length fnc x-start x-end 100))
  ([fnc x-start x-end steps] (let [--v (atom nil) fx1 (atom nil) fx2 (atom nil) length (atom nil) x1 (atom nil) x2 (atom nil)] (try (reset! x1 x-start) (reset! fx1 (fnc x-start)) (reset! length 0.0) (doseq [__i1 (py-iter (range steps))] (reset! --v __i1) (reset! x2 (py-add (py-div (py-sub x-end x-start) steps) @x1)) (reset! fx2 (fnc @x2)) (swap! length py-add (Math/hypot (py-sub @x2 @x1) (py-sub @fx2 @fx1))) (reset! x1 @x2) (reset! fx1 @fx2)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @length})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
