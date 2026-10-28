(ns clojure-algorithms.physics.speed-of-sound
  "Speed of sound in a fluid: c = sqrt(K/p) from bulk modulus and density.

  https://en.wikipedia.org/wiki/Speed_of_sound")

(defn speed-of-sound-in-a-fluid
  "Returns the speed of sound in m/s given fluid density (kg/m^3) and
  isentropic bulk modulus (Pa)."
  [density bulk-modulus]
  (cond
    (<= density 0)
    (throw (ex-info "Impossible fluid density" {:density density}))
    (<= bulk-modulus 0)
    (throw (ex-info "Impossible bulk modulus" {:bulk-modulus bulk-modulus}))
    :else
    (Math/pow (/ bulk-modulus density) 0.5)))
