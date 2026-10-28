(ns clojure-algorithms.physics.relativistic-kinetic-energy
  "Relativistic kinetic energy: K = (gamma - 1)*m*c^2.

  https://en.wikipedia.org/wiki/Kinetic_energy")

(def c
  "Speed of light in vacuum in m/s."
  299792458.0)

(defn relativistic-kinetic-energy
  "Returns the relativistic kinetic energy in Joules given rest mass (kg)
  and velocity (m/s)."
  [mass velocity]
  (when (< mass 0)
    (throw (ex-info "The mass of a body cannot be negative" {:mass mass})))
  (let [gamma (/ 1 (Math/sqrt (- 1 (/ (* velocity velocity) (* c c)))))]
    (* (- gamma 1) mass c c)))
