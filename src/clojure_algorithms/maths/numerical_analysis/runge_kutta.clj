(ns clojure-algorithms.maths.numerical-analysis.runge-kutta
  "Port of maths/numerical_analysis/runge_kutta.py")

(declare runge-kutta)

(defn runge-kutta "Calculate the numeric solution at each step to the ODE f(x, y) using RK4\n\nhttps://en.wikipedia.org/wiki/Runge-Kutta_methods\n\nArguments:\nf -- The ode as a function of x and y\ny0 -- the initial value for y\nx0 -- the initial value for x\nh -- the stepsize\nx_end -- the end value for x\n\n>>> # the exact solution is math.exp(x)\n>>> def f(x, y):\n...     return y\n>>> y0 = 1\n>>> y = runge_kutta(f, y0, 0.0, 0.01, 5)\n>>> float(y[-1])\n148.41315904125113"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: runge_kutta stubbed (np)
