(ns clojure-algorithms.maths.euler-method
  "Port of maths/euler_method.py")

(declare explicit-euler)

(defn explicit-euler "Calculate numeric solution at each step to an ODE using Euler's Method\n\nFor reference to Euler's method refer to https://en.wikipedia.org/wiki/Euler_method.\n\nArgs:\n    ode_func (Callable):  The ordinary differential equation\n        as a function of x and y.\n    y0 (float): The initial value for y.\n    x0 (float): The initial value for x.\n    step_size (float): The increment value for x.\n    x_end (float): The final value of x to be calculated.\n\nReturns:\n    np.ndarray: Solution of y for every step in x.\n\n>>> # the exact solution is math.exp(x)\n>>> def f(x, y):\n...     return y\n>>> y0 = 1\n>>> y = explicit_euler(f, y0, 0.0, 0.01, 5)\n>>> float(y[-1])\n144.77277243257308"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: explicit_euler stubbed (np)
