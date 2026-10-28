(ns clojure-algorithms.physics.malus-law
  "Malus's law for light through a polarizer: I = I0 * cos^2(theta).

  Reference: https://en.wikipedia.org/wiki/Polarizer#Malus's_law_and_other_properties")

(defn malus-law
  "Returns the transmitted intensity given initial intensity and the angle
  in degrees (0-360) between polarization direction and polarizer axis."
  [initial-intensity angle]
  (cond
    (< initial-intensity 0)
    (throw (ex-info "The value of intensity cannot be negative" {}))
    (or (< angle 0) (> angle 360))
    (throw (ex-info "In Malus Law, the angle is in the range 0-360 degrees" {}))
    :else
    (* initial-intensity (Math/pow (Math/cos (Math/toRadians angle)) 2))))
