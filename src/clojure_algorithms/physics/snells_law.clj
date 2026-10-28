(ns clojure-algorithms.physics.snells-law
  "Snell's law of refraction: n1*sin(theta1) = n2*sin(theta2).

  https://en.wikipedia.org/wiki/Snell%27s_law")

(defn calculate-refraction-angle
  "Returns the refraction angle in degrees given both refractive indices and
  the incident angle in degrees, rounded to 2 decimals. Throws ex-info when
  refraction is impossible (total internal reflection) or inputs are invalid."
  [index-of-refraction-1 index-of-refraction-2 incident-angle-degrees]
  (let [refraction-sine (* (/ index-of-refraction-1 index-of-refraction-2)
                           (Math/sin (Math/toRadians incident-angle-degrees)))]
    (cond
      (< (Math/abs (- refraction-sine 1.0)) 1e-9) 90.0
      (< (Math/abs (+ refraction-sine 1.0)) 1e-9) -90.0
      (or (< refraction-sine -1) (> refraction-sine 1))
      (throw (ex-info "Invalid physical inputs: ratio cannot be outside [-1, 1]."
                      {:sine refraction-sine}))
      :else
      (/ (double (Math/round (* (Math/toDegrees (Math/asin refraction-sine)) 100)))
         100))))
