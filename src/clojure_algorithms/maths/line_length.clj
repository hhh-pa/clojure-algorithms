(ns clojure-algorithms.maths.line-length
  "Approximate the arc length of a line segment.")

(defn line-length
  "Approximates the arc length of a line segment by treating the curve as a
  sequence of linear lines and summing their lengths."
  ([fnc x-start x-end] (line-length fnc x-start x-end 100))
  ([fnc x-start x-end steps]
   (let [dx (/ (- (double x-end) (double x-start)) steps)]
     (loop [i 0
            x1 (double x-start)
            fx1 (fnc x-start)
            length 0.0]
       (if (>= i steps)
         length
         (let [x2 (+ dx x1)
               fx2 (fnc x2)]
           (recur (inc i) x2 fx2
                  (+ length (Math/hypot (- x2 x1) (- fx2 fx1))))))))))
