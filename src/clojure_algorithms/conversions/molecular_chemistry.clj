(ns clojure-algorithms.conversions.molecular-chemistry
  "Port of conversions/molecular_chemistry.py.
  Unit conversions useful for molecular chemistry.")

(defn molarity-to-normality
  "Convert molarity (MOLES / VOLUME litres) with N-factor to normality."
  [nfactor moles volume]
  (long (Math/rint (* (double (/ moles volume)) nfactor))))

(defn moles-to-pressure
  "Convert MOLES at VOLUME litres and TEMPERATURE kelvin to atm pressure."
  [volume moles temperature]
  (long (Math/rint (/ (* moles 0.0821 temperature) volume))))

(defn moles-to-volume
  "Convert MOLES at PRESSURE atm and TEMPERATURE kelvin to litres."
  [pressure moles temperature]
  (long (Math/rint (/ (* moles 0.0821 temperature) pressure))))

(defn pressure-and-volume-to-temperature
  "Convert PRESSURE atm and VOLUME litres of MOLES to kelvin."
  [pressure moles volume]
  (long (Math/rint (/ (* pressure volume) (* 0.0821 moles)))))

(defn mass-to-moles
  "Convert MASS grams with MOLAR-MASS g/mol to moles."
  [mass molar-mass]
  (when (<= molar-mass 0)
    (throw (ex-info "Molar mass must be greater than zero."
                    {:type :value-error})))
  (when (neg? mass)
    (throw (ex-info "Mass cannot be negative." {:type :value-error})))
  (/ (Math/rint (* (/ (double mass) molar-mass) 100.0)) 100.0))

(defn moles-to-molecules
  "Convert MOLES to a molecule count via Avogadro's constant."
  [moles]
  (when (neg? moles)
    (throw (ex-info "Moles cannot be negative." {:type :value-error})))
  (Double/parseDouble (format "%.4e" (* (double moles) 6.022e23))))
