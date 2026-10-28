(ns clojure-algorithms.maths.numerical-analysis.intersection
  "Root finding with the secant method.")

(defn intersection
  "Find a root of function starting from the points x0 and x1.

  Iterates until successive approximations differ by less than 1e-5.
  Throws ex-info with :type :ZeroDivisionError when the iteration cannot
  proceed."
  [function x0 x1]
  (loop [x-n (double x0)
         x-n1 (double x1)]
    (let [f-n (double (function x-n))
          f-n1 (double (function x-n1))]
      (when (or (== x-n x-n1) (== f-n1 f-n))
        (throw (ex-info "float division by zero, could not find root"
                        {:type :ZeroDivisionError})))
      (let [x-n2 (- x-n1 (/ f-n1 (/ (- f-n1 f-n) (- x-n1 x-n))))]
        (if (< (Math/abs (- x-n2 x-n1)) 1e-5)
          x-n2
          (recur x-n1 x-n2))))))

(defn f
  "Sample function f(x) = x^3 - 2x - 5."
  [x]
  (- (Math/pow (double x) 3) (* 2 x) 5))
