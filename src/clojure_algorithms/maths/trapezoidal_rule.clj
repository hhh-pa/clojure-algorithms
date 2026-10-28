(ns clojure-algorithms.maths.trapezoidal-rule
  "Numerical integration or quadrature for a smooth function f with known values at x_i"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare trapezoidal-rule make-points f main)

(defn trapezoidal-rule "Implements the extended trapezoidal rule for numerical integration.\nThe function f(x) is provided below.\n\n:param boundary: List containing the lower and upper bounds of integration [a, b]\n:param steps: The number of steps (intervals) used in the approximation\n:return: The numerical approximation of the integral\n\n>>> abs(trapezoidal_rule([0, 1], 10) - 0.33333) < 0.01\nTrue\n>>> abs(trapezoidal_rule([0, 1], 100) - 0.33333) < 0.01\nTrue\n>>> abs(trapezoidal_rule([0, 2], 1000) - 2.66667) < 0.01\nTrue\n>>> abs(trapezoidal_rule([1, 2], 1000) - 2.33333) < 0.01\nTrue"
  [boundary steps] (let [a (atom nil) b (atom nil) h (atom nil) i (atom nil) x-i (atom nil) y (atom nil)] (try (reset! h (py-div (py-sub (py-get boundary 1) (py-get boundary 0)) steps)) (reset! a (py-get boundary 0)) (reset! b (py-get boundary 1)) (reset! x-i (make-points @a @b @h)) (reset! y 0.0) (swap! y py-add (py-mul (py-div @h 2.0) (f @a))) (doseq [__i1 (py-iter @x-i)] (reset! i __i1) (swap! y py-add (py-mul @h (f @i)))) (swap! y py-add (py-mul (py-div @h 2.0) (f @b))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @y})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn make-points "Generates points between a and b with step size h for trapezoidal integration.\n\n:param a: The lower bound of integration\n:param b: The upper bound of integration\n:param h: The step size\n:yield: The next x-value in the range (a, b)\n\n>>> list(make_points(0, 1, 0.1))    # doctest: +NORMALIZE_WHITESPACE\n[0.1, 0.2, 0.30000000000000004, 0.4, 0.5, 0.6, 0.7, 0.7999999999999999,     0.8999999999999999]\n>>> list(make_points(0, 10, 2.5))\n[2.5, 5.0, 7.5]\n>>> list(make_points(0, 10, 2))\n[2, 4, 6, 8]\n>>> list(make_points(1, 21, 5))\n[6, 11, 16]\n>>> list(make_points(1, 5, 2))\n[3]\n>>> list(make_points(1, 4, 3))\n[]"
  [a b h] (let [__ys (atom []) x (atom nil)] (try (reset! x (py-add a h)) (while (py-truthy (py-le @x (py-sub b h))) (do (swap! __ys conj @x) nil) (swap! x py-add h)) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) nil (throw __e)))) @__ys))

(defn f "This is the function to integrate, f(x) = (x - 0)^2 = x^2.\n\n:param x: The input value\n:return: The value of f(x)\n\n>>> f(0)\n0\n>>> f(1)\n1\n>>> f(0.5)\n0.25"
  [x] (py-pow x 2))

(defn main "Main function to test the trapezoidal rule.\n:a: Lower bound of integration\n:b: Upper bound of integration\n:steps: define number of steps or resolution\n:boundary: define boundary of integration\n\n>>> main()\ny = 0.3349999999999999"
  [] (let [a (atom nil) b (atom nil) boundary (atom nil) steps (atom nil) y (atom nil)] (reset! a 0.0) (reset! b 1.0) (reset! steps 10.0) (reset! boundary [@a @b]) (reset! y (trapezoidal-rule @boundary @steps)) (println (clojure.string/join " " [(py-str (str "y = " (py-str @y)))])) nil))
