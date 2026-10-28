(ns clojure-algorithms.physics.light-aberration
  "Astronomical light aberration from special relativity.

  tan(phi/2) = sqrt((1-v/c)/(1+v/c)) * tan(theta/2)
  https://en.wikipedia.org/wiki/Aberration_(astronomy)")

(defn get-aberration-angle
  "Returns the observed (aberrated) angle in radians given the rest angle
  in radians and the observer velocity as a fraction of c, rounded to 5 decimals."
  [angle-rest velocity-over-c]
  (let [factor (Math/sqrt (/ (- 1 velocity-over-c) (+ 1 velocity-over-c)))
        angle-ab (* 2 (Math/atan (* factor (Math/tan (/ angle-rest 2)))))]
    (/ (double (Math/round (* angle-ab 1e5))) 1e5)))
