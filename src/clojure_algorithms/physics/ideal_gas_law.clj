(ns clojure-algorithms.physics.ideal-gas-law
  "Ideal gas law: P*V = n*R*T.

  https://en.wikipedia.org/wiki/Ideal_gas_law")

(def universal-gas-constant
  "Universal gas constant R in J mol^-1 K^-1."
  8.314462)

(defn- check-inputs [a b c]
  (when (or (< a 0) (< b 0) (< c 0))
    (throw (ex-info "Invalid inputs. Enter positive value." {}))))

(defn pressure-of-gas-system
  "Returns pressure (Pa) given moles, temperature (K) and volume (m^3)."
  [moles kelvin volume]
  (check-inputs moles kelvin volume)
  (/ (* moles kelvin universal-gas-constant) volume))

(defn volume-of-gas-system
  "Returns volume (m^3) given moles, temperature (K) and pressure (Pa)."
  [moles kelvin pressure]
  (check-inputs moles kelvin pressure)
  (/ (* moles kelvin universal-gas-constant) pressure))

(defn temperature-of-gas-system
  "Returns temperature (K) given moles, volume (m^3) and pressure (Pa)."
  [moles volume pressure]
  (check-inputs moles volume pressure)
  (/ (* pressure volume) (* moles universal-gas-constant)))

(defn moles-of-gas-system
  "Returns amount of substance (mol) given temperature (K), volume and pressure."
  [kelvin volume pressure]
  (check-inputs kelvin volume pressure)
  (/ (* pressure volume) (* kelvin universal-gas-constant)))
