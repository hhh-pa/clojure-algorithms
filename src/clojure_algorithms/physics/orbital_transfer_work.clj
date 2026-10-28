(ns clojure-algorithms.physics.orbital-transfer-work
  "Work to move an object between orbits: W = (G*M*m/2)*(1/r1 - 1/r2).")

(def gravitational-constant
  "Gravitational constant in m^3 kg^-1 s^-2."
  6.67430e-11)

(defn orbital-transfer-work
  "Returns the work in Joules as a string in scientific notation (3 decimals)
  given central mass, object mass and initial/final orbit radii."
  [mass-central mass-object r-initial r-final]
  (when (or (<= r-initial 0) (<= r-final 0))
    (throw (ex-info "Orbital radii must be greater than zero." {})))
  (let [work (* (/ (* gravitational-constant mass-central mass-object) 2)
                (- (/ 1 r-initial) (/ 1 r-final)))]
    (format "%.3e" (double work))))
