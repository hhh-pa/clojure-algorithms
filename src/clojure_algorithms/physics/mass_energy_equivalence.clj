(ns clojure-algorithms.physics.mass-energy-equivalence
  "Einstein's mass-energy equivalence: E = m*c^2 and m = E/c^2.

  https://en.wikipedia.org/wiki/Mass%E2%80%93energy_equivalence")

(def c
  "Speed of light in vacuum in m/s."
  299792458.0)

(defn energy-from-mass
  "Returns the energy equivalent in Joules of a mass in kg."
  [mass]
  (when (< mass 0)
    (throw (ex-info "Mass can't be negative." {:mass mass})))
  (* mass c c))

(defn mass-from-energy
  "Returns the mass equivalent in kg of an energy in Joules."
  [energy]
  (when (< energy 0)
    (throw (ex-info "Energy can't be negative." {:energy energy})))
  (/ energy c c))
