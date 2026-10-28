(ns clojure-algorithms.maths.numerical-analysis.newton-raphson
  "The Newton-Raphson method (aka the Newton method) is a root-finding algorithm that\napproximates a root of a given real-valued function f(x). It is an iterative method\ngiven by the formula\n\nx_{n + 1} = x_n + f(x_n) / f'(x_n)\n\nwith the precision of the approximation increasing as the number of iterations increase.\n\nReference: https://en.wikipedia.org/wiki/Newton%27s_method"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare calc-derivative newton-raphson RealFunc)

(defn calc-derivative "Approximate the derivative of a function f(x) at a point x using the finite\ndifference method\n\n>>> import math\n>>> tolerance = 1e-5\n>>> derivative = calc_derivative(lambda x: x**2, 2)\n>>> math.isclose(derivative, 4, abs_tol=tolerance)\nTrue\n>>> derivative = calc_derivative(math.sin, 0)\n>>> math.isclose(derivative, 1, abs_tol=tolerance)\nTrue"
  ([f x] (calc-derivative f x 0.001))
  ([f x delta-x] (py-div (py-sub (f (py-add x (py-div delta-x 2))) (f (py-sub x (py-div delta-x 2)))) delta-x)))

(defn newton-raphson "Find a root of the given function f using the Newton-Raphson method.\n\n:param f: A real-valued single-variable function\n:param x0: Initial guess\n:param max_iter: Maximum number of iterations\n:param step: Step size of x, used to approximate f'(x)\n:param max_error: Maximum approximation error\n:param log_steps: bool denoting whether to log intermediate steps\n\n:return: A tuple containing the approximation, the error, and the intermediate\n    steps. If log_steps is False, then an empty list is returned for the third\n    element of the tuple.\n\n:raises ZeroDivisionError: The derivative approaches 0.\n:raises ArithmeticError: No solution exists, or the solution isn't found before the\n    iteration limit is reached.\n\n>>> import math\n>>> tolerance = 1e-15\n>>> root, *_ = newton_raphson(lambda x: x**2 - 5*x + 2, 0.4, max_error=tolerance)\n>>> math.isclose(root, (5 - math.sqrt(17)) / 2, abs_tol=tolerance)\nTrue\n>>> root, *_ = newton_raphson(lambda x: math.log(x) - 1, 2, max_error=tolerance)\n>>> math.isclose(root, math.e, abs_tol=tolerance)\nTrue\n>>> root, *_ = newton_raphson(math.sin, 1, max_error=tolerance)\n>>> math.isclose(root, 0, abs_tol=tolerance)\nTrue\n>>> newton_raphson(math.cos, 0)\nTraceback (most recent call last):\n...\nZeroDivisionError: No converging solution found, zero derivative\n>>> newton_raphson(lambda x: x**2 + 1, 2)\nTraceback (most recent call last):\n...\nArithmeticError: No converging solution found, iteration limit reached"
  ([f] (newton-raphson f 0 100 1e-06 1e-06 false))
  ([f x0] (newton-raphson f x0 100 1e-06 1e-06 false))
  ([f x0 max-iter] (newton-raphson f x0 max-iter 1e-06 1e-06 false))
  ([f x0 max-iter step] (newton-raphson f x0 max-iter step 1e-06 false))
  ([f x0 max-iter step max-error] (newton-raphson f x0 max-iter step max-error false))
  ([f x0 max-iter step max-error log-steps] (let [--v (atom nil) a (atom nil) error (atom nil) steps (atom nil)] (try (letfn [(f-derivative [x] (calc-derivative f x step))] (reset! a x0) (reset! steps []) (doseq [__i1 (py-iter (range max-iter))] (reset! --v __i1) (when (py-truthy log-steps) (swap! steps conj @a)) (reset! error (py-abs (f @a))) (when (py-truthy (py-lt @error max-error)) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@a @error @steps]}))) (when (py-truthy (py-eq (f-derivative @a) 0)) (throw (ex-info (py-str "No converging solution found, zero derivative") {:type :ZeroDivisionError}))) (swap! a py-sub (py-div (f @a) (f-derivative @a)))) (throw (ex-info (py-str "No converging solution found, iteration limit reached") {:type :ArithmeticError}))) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e)))))))

(def RealFunc nil)

;; NOTE: const RealFunc is a typing alias
