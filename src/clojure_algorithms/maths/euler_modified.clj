(ns clojure-algorithms.maths.euler-modified
  "Port of maths/euler_modified.py")

(declare euler-modified)

(defn euler-modified "Calculate solution at each step to an ODE using Euler's Modified Method\nThe Euler Method is straightforward to implement, but can't give accurate solutions.\nSo, some changes were proposed to improve accuracy.\n\nhttps://en.wikipedia.org/wiki/Euler_method\n\nArguments:\node_func -- The ode as a function of x and y\ny0 -- the initial value for y\nx0 -- the initial value for x\nstepsize -- the increment value for x\nx_end -- the end value for x\n\n>>> # the exact solution is math.exp(x)\n>>> def f1(x, y):\n...     return -2*x*(y**2)\n>>> y = euler_modified(f1, 1.0, 0.0, 0.2, 1.0)\n>>> float(y[-1])\n0.503338255442106\n>>> import math\n>>> def f2(x, y):\n...     return -2*y + (x**3)*math.exp(-2*x)\n>>> y = euler_modified(f2, 1.0, 0.0, 0.1, 0.3)\n>>> float(y[-1])\n0.5525976431951775"
  [& _args]
  ;; stub: original requires np; pure-Clojure port not applicable
  (throw (ex-info "requires np" {:type :UnsupportedOperation})))

;; NOTE: euler_modified stubbed (np)
