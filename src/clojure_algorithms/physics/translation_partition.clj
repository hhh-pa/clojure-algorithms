(ns clojure-algorithms.physics.translation-partition
  "Translational partition function.

  q-trans = ((2*pi*m*kB*T)^(3/2) * V) / h^3")

(def planck-constant
  "Planck constant in J*s."
  6.62607015e-34)

(def boltzmann-constant
  "Boltzmann constant in J/K."
  1.380649e-23)

(defn translation-partition-function
  "Returns the translational partition function given molecule mass (kg),
  temperature (K) and volume (m^3)."
  [mass temperature volume]
  (cond
    (<= mass 0) (throw (ex-info "Mass must be positive" {}))
    (<= temperature 0) (throw (ex-info "Temperature must be positive" {}))
    (<= volume 0) (throw (ex-info "Volume must be positive" {}))
    :else
    (let [prefactor (Math/pow (* 2 Math/PI mass boltzmann-constant temperature)
                              (/ 3 2))]
      (/ (* prefactor volume)
         (Math/pow planck-constant 3)))))
