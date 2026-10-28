(ns clojure-algorithms.maths.numerical-analysis.simpson-rule
  "Numerical integration or quadrature for a smooth function f with known values at x_i\n\nThis method is the classical approach of summing 'Equally Spaced Abscissas'\n\nmethod 2:\n\"Simpson Rule\""
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare method-2 make-points f main)

(defn method-2 "Calculate the definite integral of a function using Simpson's Rule.\n:param boundary: A list containing the lower and upper bounds of integration.\n:param steps: The number of steps or resolution for the integration.\n:return: The approximate integral value.\n\n>>> round(method_2([0, 2, 4], 10), 10)\n2.6666666667\n>>> round(method_2([2, 0], 10), 10)\n-0.2666666667\n>>> round(method_2([-2, -1], 10), 10)\n2.172\n>>> round(method_2([0, 1], 10), 10)\n0.3333333333\n>>> round(method_2([0, 2], 10), 10)\n2.6666666667\n>>> round(method_2([0, 2], 100), 10)\n2.5621226667\n>>> round(method_2([0, 1], 1000), 10)\n0.3320026653\n>>> round(method_2([0, 2], 0), 10)\nTraceback (most recent call last):\n    ...\nZeroDivisionError: Number of steps must be greater than zero\n>>> round(method_2([0, 2], -10), 10)\nTraceback (most recent call last):\n    ...\nZeroDivisionError: Number of steps must be greater than zero"
  [boundary steps] (let [a (atom nil) b (atom nil) cnt (atom nil) h (atom nil) i (atom nil) x-i (atom nil) y (atom nil)] (try (when (py-truthy (py-le steps 0)) (throw (ex-info (py-str "Number of steps must be greater than zero") {:type :ZeroDivisionError}))) (reset! h (py-div (py-sub (py-get boundary 1) (py-get boundary 0)) steps)) (reset! a (py-get boundary 0)) (reset! b (py-get boundary 1)) (reset! x-i (make-points @a @b @h)) (reset! y 0.0) (swap! y py-add (py-mul (py-div @h 3.0) (f @a))) (reset! cnt 2) (doseq [__i1 (py-iter @x-i)] (reset! i __i1) (swap! y py-add (py-mul (py-mul (py-div @h 3) (py-sub 4 (py-mul 2 (py-mod @cnt 2)))) (f @i))) (swap! cnt py-add 1)) (swap! y py-add (py-mul (py-div @h 3.0) (f @b))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @y})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn make-points
  [a b h] (let [__ys (atom []) x (atom nil)] (try (reset! x (py-add a h)) (while (py-truthy (py-lt @x (py-sub b h))) (do (swap! __ys conj @x) nil) (reset! x (py-add @x h))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn f
  [x] (let [y (atom nil)] (try (reset! y (py-mul (py-sub x 0) (py-sub x 0))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @y})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn main
  [] (let [a (atom nil) b (atom nil) boundary (atom nil) steps (atom nil) y (atom nil)] (reset! a 0.0) (reset! b 1.0) (reset! steps 10.0) (reset! boundary [@a @b]) (reset! y (method-2 @boundary @steps)) (println (clojure.string/join " " [(py-str (str "y = " (py-str @y)))])) nil))
