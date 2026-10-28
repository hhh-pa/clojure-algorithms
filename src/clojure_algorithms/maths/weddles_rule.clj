(ns clojure-algorithms.maths.weddles-rule
  "Port of maths/weddles_rule.py"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare get-inputs safe-function-eval compute-table apply-weights compute-solution)

(defn get-inputs "Get user input for the function, lower limit, and upper limit.\n\nReturns:\n    Tuple[str, float, float]: A tuple containing the function as a string,\n    the lower limit (a), and the upper limit (b) as floats.\n\nExample:\n    >>> from unittest.mock import patch\n    >>> inputs = ['1/(1+x**2)', 1.0, -1.0]\n    >>> with patch('builtins.input', side_effect=inputs):\n    ...     get_inputs()\n    ('1/(1+x**2)', 1.0, -1.0)"
  [] (let [func (atom nil) lower-limit (atom nil) upper-limit (atom nil)] (try (reset! func (do (print "Enter function with variable as x: ") (flush) (read-line))) (reset! lower-limit (py-float (do (print "Enter lower limit: ") (flush) (read-line)))) (reset! upper-limit (py-float (do (print "Enter upper limit: ") (flush) (read-line)))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value [@func @lower-limit @upper-limit]})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn safe-function-eval "Safely evaluates the function by substituting x value using sympy.\n\nArgs:\n    func_str (str): Function expression as a string.\n\nReturns:\n    Callable: A callable lambda function for numerical evaluation.\n\nExamples:\n    >>> f = safe_function_eval('x**2')\n    >>> f(3)\n    9\n    >>> f = safe_function_eval('x + x**2')\n    >>> f(2)\n    6"
  [& _args]
  ;; stub: original requires lambdify,symbols,sympify; pure-Clojure port not applicable
  (throw (ex-info "requires lambdify,symbols,sympify" {:type :UnsupportedOperation})))

(defn compute-table "Compute the table of function values based on the limits and accuracy.\n\nArgs:\n    func (Callable): The mathematical function as a callable.\n    lower_limit (float): The lower limit of the integral.\n    upper_limit (float): The upper limit of the integral.\n    acc (int): The number of subdivisions for accuracy.\n\nReturns:\n    Tuple[np.ndarray, float]: A tuple containing the table\n    of values and the step size (h).\n\nExample:\n    >>> compute_table(\n    ...     safe_function_eval('1/(1+x**2)'), 1, -1, 1\n    ... )\n    (array([0.5       , 0.69230769, 0.9       , 1.        , 0.9       ,\n           0.69230769, 0.5       ]), -0.3333333333333333)"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

(defn apply-weights "Apply Weddle's rule weights to the values in the table.\n\nArgs:\n    table (List[float]): A list of computed function values.\n\nReturns:\n    List[float]: A list of weighted values.\n\nExample:\n    >>> apply_weights([0.0, 0.866, 1.0, 0.866, 0.0, -0.866, -1.0])\n    [4.33, 1.0, 5.196, 0.0, -4.33]"
  [table] (let [add (atom nil) i (atom nil)] (try (reset! add []) (doseq [__i1 (py-iter (range 1 (py-sub (count table) 1)))] (reset! i __i1) (if (py-truthy (let [__b2 (py-eq (py-mod @i 2) 0)] (if (py-truthy __b2) (not (py-eq (py-mod @i 3) 0)) __b2))) (do (swap! add conj (py-get table @i))) (do (if (py-truthy (let [__b3 (not (py-eq (py-mod @i 2) 0))] (if (py-truthy __b3) (not (py-eq (py-mod @i 3) 0)) __b3))) (do (swap! add conj (py-mul 5 (py-get table @i)))) (do (if (py-truthy (py-eq (py-mod @i 6) 0)) (do (swap! add conj (py-mul 2 (py-get table @i)))) (do (when (py-truthy (let [__b4 (py-eq (py-mod @i 3) 0)] (if (py-truthy __b4) (not (py-eq (py-mod @i 2) 0)) __b4))) (swap! add conj (py-mul 6 (py-get table @i))))))))))) (throw (ex-info "__return__" {:__control__ true :__return__ true :value @add})) (catch clojure.lang.ExceptionInfo __e (if (:__return__ (ex-data __e)) (:value (ex-data __e)) (throw __e))))))

(defn compute-solution "Compute the final solution using the weighted values and table.\n\nArgs:\n    add (List[float]): A list of weighted values from apply_weights.\n    table (List[float]): A list of function values.\n    step_size (float): The step size calculated from the limits and accuracy.\n\nReturns:\n    float: The final computed integral solution.\n\nExample:\n    >>> compute_solution([4.33, 6.0, 0.0, -4.33], [0.0, 0.866, 1.0, 0.866, 0.0,\n    ... -0.866, -1.0], 0.5235983333333333)\n    0.7853975"
  [add table step-size] (py-mul (py-mul 0.3 step-size) (py-add (py-add (reduce +' 0 add) (py-get table 0)) (py-get table (py-neg 1)))))

;; NOTE: safe_function_eval stubbed (lambdify,symbols,sympify)
;; NOTE: compute_table stubbed (np)
