(ns clojure-algorithms.maths.numerical-analysis.numerical-integration
  "Approximates the area under the curve using the trapezoidal rule")

(defn trapezoidal-area
  "Treat curve as a collection of linear lines and sum the area of the
trapezium shapes they form.

  fnc defines the curve, x-start and x-end delimit the segment, and steps
  gauges accuracy (default 100). Returns the approximated area."
  ([fnc x-start x-end] (trapezoidal-area fnc x-start x-end 100))
  ([fnc x-start x-end steps]
   (let [dx (/ (- (double x-end) (double x-start)) steps)]
     (loop [x1 (double x-start)
            fx1 (double (fnc x-start))
            area 0.0
            remaining steps]
       (if (zero? remaining)
         area
         (let [x2 (+ dx x1)
               fx2 (double (fnc x2))]
           (recur x2 fx2
                  (+ area (/ (* (Math/abs (+ fx2 fx1)) (- x2 x1)) 2))
                  (dec remaining))))))))
