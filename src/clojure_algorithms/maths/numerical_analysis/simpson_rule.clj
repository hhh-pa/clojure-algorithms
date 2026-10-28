(ns clojure-algorithms.maths.numerical-analysis.simpson-rule
  "Numerical integration or quadrature for a smooth function f with known values at x_i

This method is the classical approach of summing 'Equally Spaced Abscissas'

method 2:
\"Simpson Rule\"")

(defn f
  "Sample function f(x) = x^2."
  [x]
  (* (- x 0) (- x 0)))

(defn make-points
  "Return the interior sample points between a and b spaced h apart."
  [a b h]
  (loop [x (+ (double a) (double h))
         points []]
    (if (< x (- (double b) (double h)))
      (recur (+ x (double h)) (conj points x))
      points)))

(defn method-2
  "Calculate the definite integral of f using Simpson's Rule.

  boundary holds the lower and upper bounds of integration, steps the
  resolution. Throws ex-info with :type :ZeroDivisionError when steps is not
  positive."
  [boundary steps]
  (when (<= steps 0)
    (throw (ex-info "Number of steps must be greater than zero"
                    {:type :ZeroDivisionError})))
  (let [a (nth boundary 0)
        b (nth boundary 1)
        h (/ (- (double b) (double a)) (double steps))]
    (+ (* (/ h 3.0) (f a))
       (reduce (fn [y [i x]]
                 (+ y (* (/ h 3) (- 4 (* 2 (mod (+ i 2) 2))) (f x))))
               0.0
               (map-indexed vector (make-points a b h)))
       (* (/ h 3.0) (f b)))))

(defn main
  "Integrate f from 0 to 1 with 10 steps and print the result."
  []
  (let [a 0.0
        b 1.0
        steps 10.0
        boundary [a b]
        y (method-2 boundary steps)]
    (println (str "y = " y))))
