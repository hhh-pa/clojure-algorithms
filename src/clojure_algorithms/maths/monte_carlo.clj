(ns clojure-algorithms.maths.monte-carlo
  "Monte Carlo method estimations.")

(defn- uniform
  [low high]
  (+ low (* (rand) (- high low))))

(defn- mean
  [xs]
  (/ (reduce + xs) (double (count xs))))

(defn pi-estimator
  "Estimate pi by sampling dots in a 2x2 square; prints the estimate."
  [iterations]
  (let [in-circle? (fn [x y]
                     (<= (Math/sqrt (+ (* x x) (* y y))) 1))
        proportion (mean (repeatedly iterations
                                     #(if (in-circle? (uniform -1.0 1.0)
                                                      (uniform -1.0 1.0))
                                        1 0)))
        pi-estimate (* proportion 4)]
    (println (str "The estimated value of pi is " pi-estimate))
    (println (str "The numpy value of pi is " Math/PI))
    (println (str "The total error is " (abs (- Math/PI pi-estimate)))))
  nil)

(defn area-under-curve-estimator
  "Estimate the integral of function-to-integrate over [min-value, max-value]."
  ([iterations function-to-integrate]
   (area-under-curve-estimator iterations function-to-integrate 0.0 1.0))
  ([iterations function-to-integrate min-value]
   (area-under-curve-estimator iterations function-to-integrate min-value 1.0))
  ([iterations function-to-integrate min-value max-value]
   (* (mean (repeatedly iterations
                        #(function-to-integrate (uniform min-value max-value))))
      (- max-value min-value))))

(defn area-under-line-estimator-check
  "Check the estimation error for f(x) = x; prints estimated/expected/error."
  ([iterations] (area-under-line-estimator-check iterations 0.0 1.0))
  ([iterations min-value] (area-under-line-estimator-check iterations min-value 1.0))
  ([iterations min-value max-value]
   (let [estimated-value (area-under-curve-estimator iterations identity
                                                     min-value max-value)
         expected-value (/ (- (* max-value max-value) (* min-value min-value)) 2)]
     (println "******************")
     (println (str "Estimating area under y=x where x varies from "
                   min-value " to " max-value))
     (println (str "Estimated value is " estimated-value))
     (println (str "Expected value is " expected-value))
     (println (str "Total error is " (abs (- estimated-value expected-value))))
     (println "******************"))
   nil))

(defn pi-estimator-using-area-under-curve
  "Estimate pi as the area under y = sqrt(4 - x^2) over [0, 2]; prints it."
  [iterations]
  (let [estimated-value (area-under-curve-estimator
                         iterations
                         (fn [x] (Math/sqrt (- 4.0 (* x x))))
                         0.0 2.0)]
    (println "******************")
    (println "Estimating pi using area_under_curve_estimator")
    (println (str "Estimated value is " estimated-value))
    (println (str "Expected value is " Math/PI))
    (println (str "Total error is " (abs (- estimated-value Math/PI))))
    (println "******************"))
  nil)
