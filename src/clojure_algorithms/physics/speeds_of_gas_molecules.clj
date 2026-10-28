(ns clojure-algorithms.physics.speeds-of-gas-molecules
  "Average and most-probable gas molecule speeds from Maxwell-Boltzmann theory.

  v-avg = sqrt(8*R*T/(pi*M)), v-mp = sqrt(2*R*T/M)
  https://en.wikipedia.org/wiki/Maxwell%E2%80%93Boltzmann_distribution")

(def gas-constant
  "Molar gas constant R in J mol^-1 K^-1."
  8.31446261815324)

(defn avg-speed-of-molecule
  "Returns the average molecular speed in m/s given temperature (K) and
  molar mass (kg/mol)."
  [temperature molar-mass]
  (cond
    (< temperature 0)
    (throw (ex-info "Absolute temperature cannot be less than 0 K" {}))
    (<= molar-mass 0)
    (throw (ex-info "Molar mass should be greater than 0 kg/mol" {}))
    :else
    (Math/pow (/ (* 8 gas-constant temperature) (* Math/PI molar-mass)) 0.5)))

(defn mps-speed-of-molecule
  "Returns the most probable molecular speed in m/s given temperature (K)
  and molar mass (kg/mol)."
  [temperature molar-mass]
  (cond
    (< temperature 0)
    (throw (ex-info "Absolute temperature cannot be less than 0 K" {}))
    (<= molar-mass 0)
    (throw (ex-info "Molar mass should be greater than 0 kg/mol" {}))
    :else
    (Math/pow (/ (* 2 gas-constant temperature) molar-mass) 0.5)))
