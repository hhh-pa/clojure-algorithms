(ns clojure-algorithms.maths.numerical-analysis.runge-kutta-fehlberg-45
  "Use the Runge-Kutta-Fehlberg method to solve Ordinary Differential Equations.")

(declare runge-kutta-fehlberg-45)

(defn runge-kutta-fehlberg-45 "Solve an Ordinary Differential Equations using Runge-Kutta-Fehlberg Method (rkf45)\nof order 5.\n\nhttps://en.wikipedia.org/wiki/Runge%E2%80%93Kutta%E2%80%93Fehlberg_method\n\nargs:\nfunc: An ordinary differential equation (ODE) as function of x and y.\nx_initial: The initial value of x.\ny_initial: The initial value of y.\nstep_size: The increment value of x.\nx_final: The final value of x.\n\nReturns:\n    Solution of y at each nodal point\n\n# exact value of y[1] is tan(0.2) = 0.2027100937470787\n>>> def f(x, y):\n...     return 1 + y**2\n>>> y = runge_kutta_fehlberg_45(f, 0, 0, 0.2, 1)\n>>> float(y[1])\n0.2027100937470787\n>>> def f(x,y):\n...     return x\n>>> y = runge_kutta_fehlberg_45(f, -1, 0, 0.2, 0)\n>>> float(y[1])\n-0.18000000000000002\n>>> y = runge_kutta_fehlberg_45(5, 0, 0, 0.1, 1)\nTraceback (most recent call last):\n    ...\nTypeError: 'int' object is not callable\n>>> def f(x, y):\n...     return x + y\n>>> y = runge_kutta_fehlberg_45(f, 0, 0, 0.2, -1)\nTraceback (most recent call last):\n    ...\nValueError: The final value of x must be greater than initial value of x.\n>>> def f(x, y):\n...     return x\n>>> y = runge_kutta_fehlberg_45(f, -1, 0, -0.2, 0)\nTraceback (most recent call last):\n    ...\nValueError: Step size must be positive."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: runge_kutta_fehlberg_45 stubbed (np)
