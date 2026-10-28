(ns clojure-algorithms.maths.sin
  "Calculate sin function.

  Formula: sin(x) = x - x^3/3! + x^5/5! - x^7/7! + ...
  Where: x = angle in radians."
  (:import [java.math BigDecimal RoundingMode]))

(defn- factorial
  [n]
  (reduce *' 1 (range 1 (inc n))))

(defn- round-half-even
  [x n]
  (double (.setScale (BigDecimal/valueOf x) n RoundingMode/HALF_EVEN)))

(defn sin
  "Implement sin function via its Taylor series (angle in degrees)."
  ([angle-in-degrees] (sin angle-in-degrees 18 10))
  ([angle-in-degrees accuracy] (sin angle-in-degrees accuracy 10))
  ([angle-in-degrees accuracy rounded-values-count]
   (let [angle (- angle-in-degrees (* (Math/floor (/ angle-in-degrees 360.0)) 360.0))
         radians (Math/toRadians angle)]
     (loop [i 0
            result radians
            a 3
            b -1]
       (if (>= i accuracy)
         (round-half-even result rounded-values-count)
         (recur (inc i)
                (+ result (/ (* b (Math/pow radians a)) (double (factorial a))))
                (+ a 2)
                (- b)))))))
