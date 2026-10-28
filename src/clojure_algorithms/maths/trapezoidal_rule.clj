(ns clojure-algorithms.maths.trapezoidal-rule
  "Numerical integration or quadrature for a smooth function f with known values at x_i")

(defn f
  "The function to integrate: f(x) = x^2."
  [x]
  (* x x))

(defn make-points
  "Generate the interior points between a and b with step size h,
  as a vector."
  [a b h]
  (loop [x (+ a h)
         points []]
    (if (<= x (- b h))
      (recur (+ x h) (conj points x))
      points)))

(defn trapezoidal-rule
  "Approximate the integral of f over [a, b] with the extended
  trapezoidal rule using the given number of steps. boundary is [a b]."
  [boundary steps]
  (let [a (nth boundary 0)
        b (nth boundary 1)
        h (/ (double (- b a)) (double steps))
        y (+ 0.0 (* (/ h 2.0) (f a)))
        y (reduce (fn [acc i] (+ acc (* h (f i))))
                  y
                  (make-points a b h))]
    (+ y (* (/ h 2.0) (f b)))))

(defn main
  "Integrate f over [0, 1] with 10 steps and print the result."
  []
  (let [a 0.0
        b 1.0
        steps 10.0
        y (trapezoidal-rule [a b] steps)]
    (println (str "y = " y))))
