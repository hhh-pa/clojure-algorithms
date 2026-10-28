(ns clojure-algorithms.physics.altitude-pressure
  "Approximate altitude from atmospheric pressure using the barometric formula.

  H = 44330 * [1 - (P/p0)^(1/5.5255)]
  Reference pressure at sea level p0 = 101325 Pa.")

(defn get-altitude-at-pressure
  "Calculates the altitude in meters from pressure in Pascals.
  Throws ex-info when pressure is negative or above sea-level pressure."
  [pressure]
  (cond
    (> pressure 101325)
    (throw (ex-info "Value Higher than Pressure at Sea Level !" {:pressure pressure}))
    (< pressure 0)
    (throw (ex-info "Atmospheric Pressure can not be negative !" {:pressure pressure}))
    :else
    (* 44330 (- 1 (Math/pow (/ pressure 101325.0) (/ 1 5.5255))))))
