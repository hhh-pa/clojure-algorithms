(ns clojure-algorithms.maths.numerical-analysis.newton-raphson
  "The Newton-Raphson method (aka the Newton method) is a root-finding algorithm that
approximates a root of a given real-valued function f(x). It is an iterative method
given by the formula

x_{n + 1} = x_n + f(x_n) / f'(x_n)

with the precision of the approximation increasing as the number of iterations increase.

Reference: https://en.wikipedia.org/wiki/Newton%27s_method")

(def RealFunc nil)

(defn calc-derivative
  "Approximate the derivative of f at x with the finite difference method."
  ([f x] (calc-derivative f x 1e-3))
  ([f x delta-x]
   (/ (- (f (+ x (/ delta-x 2))) (f (- x (/ delta-x 2)))) delta-x)))

(defn newton-raphson
  "Find a root of f using the Newton-Raphson method.

  x0 is the initial guess, max-iter the maximum number of iterations, step
  the step size used to approximate f'(x), max-error the maximum
  approximation error, and log-steps whether to log intermediate steps.

  Returns a vector of the approximation, the error, and the intermediate
  steps (empty when log-steps is false). Throws ex-info with
  :type :ZeroDivisionError when the derivative is zero, and with
  :type :ArithmeticError when the iteration limit is reached."
  ([f] (newton-raphson f 0 100 1e-6 1e-6 false))
  ([f x0] (newton-raphson f x0 100 1e-6 1e-6 false))
  ([f x0 max-iter] (newton-raphson f x0 max-iter 1e-6 1e-6 false))
  ([f x0 max-iter step] (newton-raphson f x0 max-iter step 1e-6 false))
  ([f x0 max-iter step max-error]
   (newton-raphson f x0 max-iter step max-error false))
  ([f x0 max-iter step max-error log-steps]
   (let [f-derivative (fn [x] (calc-derivative f x step))]
     (loop [a x0
            steps []
            iter 0]
       (if (>= iter max-iter)
         (throw (ex-info "No converging solution found, iteration limit reached"
                         {:type :ArithmeticError}))
         (let [steps (if log-steps (conj steps a) steps)
               error (Math/abs (double (f a)))]
           (cond
             (< error max-error)
             [a error steps]

             (zero? (f-derivative a))
             (throw (ex-info "No converging solution found, zero derivative"
                             {:type :ZeroDivisionError}))

             :else
             (recur (- a (/ (f a) (f-derivative a))) steps (inc iter)))))))))
