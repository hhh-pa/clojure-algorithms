(ns clojure-algorithms.maths.numerical-analysis.adams-bashforth
  "Use the Adams-Bashforth methods to solve Ordinary Differential Equations.

https://en.wikipedia.org/wiki/Linear_multistep_method
Author : Ravi Kumar")

(defrecord AdamsBashforth [func x-initials y-initials step-size x-final])

(defn AdamsBashforth-post-init
  "Validate an AdamsBashforth configuration.

  Throws ex-info with :type :ValueError when the final x is not greater
  than the last initial x, when the step size is not positive, or when the
  initial x values are not equally spaced according to the step size."
  [self]
  (let [{:keys [x-initials step-size x-final]} self
        round10 (fn [v] (/ (Math/round (* (double v) 1e10)) 1e10))]
    (when (>= (last x-initials) x-final)
      (throw (ex-info "The final value of x must be greater than the initial values of x."
                      {:type :ValueError})))
    (when (<= step-size 0)
      (throw (ex-info "Step size must be positive." {:type :ValueError})))
    (when-not (every? (fn [[x0 x1]] (== (round10 (- x1 x0)) step-size))
                      (map vector x-initials (rest x-initials)))
      (throw (ex-info "x-values must be equally spaced according to step size."
                      {:type :ValueError})))
    nil))

(defn make-AdamsBashforth
  "Constructor for AdamsBashforth.

  func: An ordinary differential equation (ODE) as function of x and y.
  x-initials: initial values of x.
  y-initials: initial values of y.
  step-size: the increment value of x.
  x-final: the final value of x.

  Returns the validated configuration record."
  [func x-initials y-initials step-size x-final]
  (let [self (->AdamsBashforth func (vec x-initials) (vec y-initials)
                               step-size x-final)]
    (AdamsBashforth-post-init self)
    self))

(defn- check-initial-count
  [self expected-count]
  (let [{:keys [x-initials y-initials]} self]
    (when (or (not= expected-count (count x-initials))
              (not= expected-count (count y-initials)))
      (throw (ex-info "Insufficient initial points information."
                      {:type :ValueError})))))

(defn AdamsBashforth-step-2
  "Solve with the 2-step Adams-Bashforth method.

  Returns a vector with the solution of y at each nodal point."
  [self]
  (check-initial-count self 2)
  (let [{:keys [func step-size x-final]} self
        [x0 x1] (:x-initials self)
        [y0 y1] (:y-initials self)
        n (int (/ (- x-final x1) step-size))]
    (loop [i 0
           x0 (double x0)
           x1 (double x1)
           y (assoc (vec (repeat (+ n 2) 0.0)) 0 (double y0) 1 (double y1))]
      (if (>= i n)
        y
        (recur (inc i)
               x1
               (+ x1 step-size)
               (assoc y (+ i 2)
                      (+ (nth y (inc i))
                         (* (/ step-size 2)
                            (- (* 3 (func x1 (nth y (inc i))))
                               (func x0 (nth y i)))))))))))

(defn AdamsBashforth-step-3
  "Solve with the 3-step Adams-Bashforth method.

  Returns a vector with the solution of y at each nodal point."
  [self]
  (check-initial-count self 3)
  (let [{:keys [func step-size x-final]} self
        [x0 x1 x2] (:x-initials self)
        [y0 y1 y2] (:y-initials self)
        n (int (/ (- x-final x2) step-size))]
    (loop [i 0
           x0 (double x0)
           x1 (double x1)
           x2 (double x2)
           y (assoc (vec (repeat (+ n 4) 0.0))
                    0 (double y0) 1 (double y1) 2 (double y2))]
      (if (> i n)
        y
        (recur (inc i)
               x1
               x2
               (+ x2 step-size)
               (assoc y (+ i 3)
                      (+ (nth y (+ i 2))
                         (* (/ step-size 12)
                            (+ (* 23 (func x2 (nth y (+ i 2))))
                               (* -16 (func x1 (nth y (inc i))))
                               (* 5 (func x0 (nth y i))))))))))))

(defn AdamsBashforth-step-4
  "Solve with the 4-step Adams-Bashforth method.

  Returns a vector with the solution of y at each nodal point."
  [self]
  (check-initial-count self 4)
  (let [{:keys [func step-size x-final]} self
        [x0 x1 x2 x3] (:x-initials self)
        [y0 y1 y2 y3] (:y-initials self)
        n (int (/ (- x-final x3) step-size))]
    (loop [i 0
           x0 (double x0)
           x1 (double x1)
           x2 (double x2)
           x3 (double x3)
           y (assoc (vec (repeat (+ n 4) 0.0))
                    0 (double y0) 1 (double y1) 2 (double y2) 3 (double y3))]
      (if (>= i n)
        y
        (recur (inc i)
               x1
               x2
               x3
               (+ x3 step-size)
               (assoc y (+ i 4)
                      (+ (nth y (+ i 3))
                         (* (/ step-size 24)
                            (+ (* 55 (func x3 (nth y (+ i 3))))
                               (* -59 (func x2 (nth y (+ i 2))))
                               (* 37 (func x1 (nth y (inc i))))
                               (* -9 (func x0 (nth y i))))))))))))

(defn AdamsBashforth-step-5
  "Solve with the 5-step Adams-Bashforth method.

  Returns a vector with the solution of y at each nodal point."
  [self]
  (check-initial-count self 5)
  (let [{:keys [func step-size x-final]} self
        [x0 x1 x2 x3 x4] (:x-initials self)
        [y0 y1 y2 y3 y4] (:y-initials self)
        n (int (/ (- x-final x4) step-size))]
    (loop [i 0
           x0 (double x0)
           x1 (double x1)
           x2 (double x2)
           x3 (double x3)
           x4 (double x4)
           y (assoc (vec (repeat (+ n 6) 0.0))
                    0 (double y0) 1 (double y1) 2 (double y2)
                    3 (double y3) 4 (double y4))]
      (if (> i n)
        y
        (recur (inc i)
               x1
               x2
               x3
               x4
               (+ x4 step-size)
               (assoc y (+ i 5)
                      (+ (nth y (+ i 4))
                         (* (/ step-size 720)
                            (+ (* 1901 (func x4 (nth y (+ i 4))))
                               (* -2774 (func x3 (nth y (+ i 3))))
                               (* -2616 (func x2 (nth y (+ i 2))))
                               (* -1274 (func x1 (nth y (inc i))))
                               (* 251 (func x0 (nth y i))))))))))))
