(ns clojure-algorithms.maths.numerical-analysis.secant-method
  "Implementing Secant method in Python
Author: dimgrichr")

(defn f
  "Sample function f(x) = 8x - 2e^(-x)."
  [x]
  (- (* 8 x) (* 2 (Math/exp (double (- x))))))

(defn secant-method
  "Approximate a root of f with the secant method.

  Starts from lower-bound and upper-bound and iterates repeats times."
  [lower-bound upper-bound repeats]
  (loop [x0 lower-bound
         x1 upper-bound
         remaining repeats]
    (if (zero? remaining)
      x1
      (recur x1
             (- x1 (/ (* (f x1) (- x1 x0)) (- (f x1) (f x0))))
             (dec remaining)))))
