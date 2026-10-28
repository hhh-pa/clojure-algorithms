(ns clojure-algorithms.physics.kinetic-energy
  "Classical kinetic energy of a non-rotating object: K = m*v^2/2.

  https://en.m.wikipedia.org/wiki/Kinetic_energy")

(defn kinetic-energy
  "Returns the kinetic energy in Joules given mass (kg) and velocity (m/s)."
  [mass velocity]
  (when (< mass 0)
    (throw (ex-info "The mass of a body cannot be negative" {:mass mass})))
  (* 0.5 mass (Math/abs (double velocity)) (Math/abs (double velocity))))
