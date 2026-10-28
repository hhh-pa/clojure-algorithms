(ns clojure-algorithms.physics.magnetic-flux
  "Magnetic flux through a closed area: Phi = B*A*cos(theta).

  https://en.wikipedia.org/wiki/Magnetic_flux")

(defn- check-args
  "Validates magnetic field, area and angle. Throws on invalid input."
  [magnetic-field area angle]
  (when (not (number? magnetic-field))
    (throw (ex-info "Invalid magnetic field. Should be an integer or float." {})))
  (when (not (number? area))
    (throw (ex-info "Invalid area. Should be an integer or float." {})))
  (when (not (number? angle))
    (throw (ex-info "Invalid angle. Should be an integer or float." {})))
  (when (or (< angle 0) (> angle 180))
    (throw (ex-info "Invalid angle. Range is 0-180 degrees." {:angle angle})))
  (when (< magnetic-field 0)
    (throw (ex-info "Invalid magnetic field. Should be a positive number." {})))
  (when (< area 0)
    (throw (ex-info "Invalid area. Should be a positive number." {}))))

(defn magnetic-flux
  "Returns the magnetic flux in Wb given field (T), area (m^2) and angle
  between field and normal (degrees), rounded to 1 decimal."
  [magnetic-field area angle]
  (check-args magnetic-field area angle)
  (/ (double (Math/round (* (* magnetic-field area
                               (Math/cos (Math/toRadians angle)))
                            10)))
     10))
