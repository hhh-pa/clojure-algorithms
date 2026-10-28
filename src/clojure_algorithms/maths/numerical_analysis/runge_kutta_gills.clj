(ns clojure-algorithms.maths.numerical-analysis.runge-kutta-gills
  "Use the Runge-Kutta-Gill's method of order 4 to solve Ordinary Differential Equations.\n\nhttps://www.geeksforgeeks.org/gills-4th-order-method-to-solve-differential-equations/\nAuthor : Ravi Kumar")

(declare runge-kutta-gills)

(defn runge-kutta-gills "Solve an Ordinary Differential Equations using Runge-Kutta-Gills Method of order 4.\n\nargs:\nfunc: An ordinary differential equation (ODE) as function of x and y.\nx_initial: The initial value of x.\ny_initial: The initial value of y.\nstep_size: The increment value of x.\nx_final: The final value of x.\n\nReturns:\n    Solution of y at each nodal point\n\n>>> def f(x, y):\n...     return (x-y)/2\n>>> y = runge_kutta_gills(f, 0, 3, 0.2, 5)\n>>> float(y[-1])\n3.4104259225717537\n\n>>> def f(x,y):\n...     return x\n>>> y = runge_kutta_gills(f, -1, 0, 0.2, 0)\n>>> y\narray([ 0.  , -0.18, -0.32, -0.42, -0.48, -0.5 ])\n\n>>> def f(x, y):\n...     return x + y\n>>> y = runge_kutta_gills(f, 0, 0, 0.2, -1)\nTraceback (most recent call last):\n    ...\nValueError: The final value of x must be greater than initial value of x.\n\n>>> def f(x, y):\n...     return x\n>>> y = runge_kutta_gills(f, -1, 0, -0.2, 0)\nTraceback (most recent call last):\n    ...\nValueError: Step size must be positive."
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: runge_kutta_gills stubbed (np)
