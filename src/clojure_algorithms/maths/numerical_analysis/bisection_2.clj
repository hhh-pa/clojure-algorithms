(ns clojure-algorithms.maths.numerical-analysis.bisection-2
  "Given a function on floating number f(x) and two floating numbers `a` and `b` such that\nf(a) * f(b) < 0 and f(x) is continuous in [a, b].\nHere f(x) represents algebraic or transcendental equation.\nFind root of function in interval [a, b] (Or find a value of x such that f(x) is 0)\n\nhttps://en.wikipedia.org/wiki/Bisection_method"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare equation bisection)

(defn equation ">>> equation(5)\n-15\n>>> equation(0)\n10\n>>> equation(-5)\n-15\n>>> equation(0.1)\n9.99\n>>> equation(-0.1)\n9.99"
  [x] (py-sub 10 (py-mul x x)))

(defn bisection ">>> bisection(-2, 5)\n3.1611328125\n>>> bisection(0, 6)\n3.158203125\n>>> bisection(2, 3)\nTraceback (most recent call last):\n    ...\nValueError: Wrong space!"
  [a b] (let [a (atom a) b (atom b)] (let [c (atom nil)] (try (when (py-truthy (py-ge (py-mul (equation @a) (equation @b)) 0)) (throw (ex-info (py-str "Wrong space!") {:type :ValueError}))) (reset! c @a) (try (while (py-truthy (py-ge (py-sub @b @a) 0.01)) (reset! c (py-div (py-add @a @b) 2)) (when (py-truthy (py-eq (equation @c) 0.0)) (throw (ex-info "__break__" {:__control__ true :__break__ true}))) (if (py-truthy (py-lt (py-mul (equation @c) (equation @a)) 0)) (do (reset! b @c)) (do (reset! a @c)))) (catch clojure.lang.ExceptionInfo __e (if (:__break__ (ex-data __e)) nil (throw __e)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @c})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))
