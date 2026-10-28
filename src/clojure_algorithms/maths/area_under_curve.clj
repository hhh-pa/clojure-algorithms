(ns clojure-algorithms.maths.area-under-curve
  "Approximates the area under the curve using the trapezoidal rule.")

(defn trapezoidal-area
  "Treats curve as a collection of linear lines and sums the area of the
  trapezium shape they form."
  ([fnc x-start x-end] (trapezoidal-area fnc x-start x-end 100))
  ([fnc x-start x-end steps]
   (let [dx (/ (- (double x-end) (double x-start)) steps)]
     (loop [i 0
            x1 (double x-start)
            fx1 (fnc x-start)
            area 0.0]
       (if (>= i steps)
         area
         (let [x2 (+ dx x1)
               fx2 (fnc x2)]
           (recur (inc i) x2 fx2
                  (+ area (/ (* (abs (+ fx2 fx1)) (- x2 x1)) 2)))))))))
