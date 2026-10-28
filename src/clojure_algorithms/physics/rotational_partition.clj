(ns clojure-algorithms.physics.rotational-partition
  "Rotational partition function for linear molecules.

  q-rot = (2*pi*I*kB*T) / h^2")

(def boltzmann-constant
  "Boltzmann constant in J/K."
  1.380649e-23)

(def planck-constant
  "Planck constant in J*s."
  6.62607015e-34)

(defn rotation-partition-function
  "Returns the rotational partition function given moment of inertia (kg*m^2)
  and temperature (K)."
  [moment-of-inertia temperature]
  (cond
    (<= moment-of-inertia 0)
    (throw (ex-info "Moment of inertia must be positive" {}))
    (<= temperature 0)
    (throw (ex-info "Temperature must be positive" {}))
    :else
    (/ (* 2 Math/PI moment-of-inertia boltzmann-constant temperature)
       (* planck-constant planck-constant))))
