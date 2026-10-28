(ns clojure-algorithms.physics.rms-speed-of-molecule
  "Root-mean-square speed of gas molecules: v-rms = sqrt(3*R*T/M).")

(def universal-gas-constant
  "Universal gas constant R in J mol^-1 K^-1."
  8.3144598)

(defn rms-speed-of-molecule
  "Returns the RMS speed in m/s given temperature (K) and molar mass (kg/mol)."
  [temperature molar-mass]
  (cond
    (< temperature 0)
    (throw (ex-info "Temperature cannot be less than 0 K"
                    {:temperature temperature}))
    (<= molar-mass 0)
    (throw (ex-info "Molar mass cannot be less than or equal to 0 kg/mol"
                    {:molar-mass molar-mass}))
    :else
    (Math/pow (/ (* 3 universal-gas-constant temperature) molar-mass) 0.5)))
