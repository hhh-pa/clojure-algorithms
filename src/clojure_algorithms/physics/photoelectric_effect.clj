(ns clojure-algorithms.physics.photoelectric-effect
  "Photoelectric effect: max kinetic energy K-max = h*f - W.

  https://en.wikipedia.org/wiki/Photoelectric_effect")

(def planck-constant-js
  "Planck constant in Joule-seconds."
  (* 6.6261 (Math/pow 10 -34)))

(def planck-constant-evs
  "Planck constant in eV-seconds."
  (* 4.1357 (Math/pow 10 -15)))

(defn maximum-kinetic-energy
  "Returns the max kinetic energy of the emitted electron given wave
  frequency and surface work function (pass in-ev? true for eV units).
  Zero means no electron is emitted."
  ([frequency work-function] (maximum-kinetic-energy frequency work-function false))
  ([frequency work-function in-ev?]
   (when (< frequency 0)
     (throw (ex-info "Frequency can't be negative." {:frequency frequency})))
   (let [h (if in-ev? planck-constant-evs planck-constant-js)]
     (max (- (* h frequency) work-function) 0))))
