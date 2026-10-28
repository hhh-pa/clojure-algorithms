(ns clojure-algorithms.physics.de-broglie
  "de Broglie wavelength of a particle: lambda = h/p.")

(def planck-constant-js
  "Planck constant in Joule-seconds."
  6.62607015e-34)

(def planck-constant-evs
  "Planck constant in eV-seconds."
  4.135667696e-15)

(defn de-broglie-wavelength
  "Returns the de Broglie wavelength in meters as a string in scientific
  notation (8 decimals) given particle momentum (in kg*m/s, or eV*s when
  in-ev? is true)."
  ([momentum] (de-broglie-wavelength momentum false))
  ([momentum in-ev?]
   (when (<= momentum 0)
     (throw (ex-info "Momentum can't be zero or negative."
                     {:momentum momentum})))
   (let [h (if in-ev? planck-constant-evs planck-constant-js)]
     (format "%.8e" (double (/ h momentum))))))
