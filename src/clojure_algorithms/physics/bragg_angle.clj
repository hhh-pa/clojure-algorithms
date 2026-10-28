(ns clojure-algorithms.physics.bragg-angle
  "Bragg diffraction angle: sin(theta) = (n * lambda) / (2 * d).")

(defn bragg-angle
  "Returns the Bragg diffraction angle in degrees, rounded to 1 decimal,
  given crystal plane distance (m), reflection order and wavelength (m)."
  [distance order wavelength]
  (let [sine-theta (/ (* order wavelength) (* 2 distance))]
    (when (or (> sine-theta 1) (< sine-theta -1))
      (throw (ex-info "The calculated sine value is out of the valid range."
                      {:sine sine-theta})))
    (let [s (Math/pow 10 1)]
      (/ (double (Math/round (* (Math/toDegrees (Math/asin sine-theta)) s))) s))))
