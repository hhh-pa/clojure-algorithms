(ns clojure-algorithms.physics.casimir-effect
  "Casimir force between uncharged parallel conducting plates in vacuum.

  F = (hbar * c * pi^2 * A) / (240 * a^4)  (magnitude only).
  https://en.wikipedia.org/wiki/Casimir_effect")

(def reduced-planck-constant
  "Reduced Planck constant hbar in J*s."
  1.054571817e-34)

(def speed-of-light
  "Speed of light in m/s."
  3e8)

(defn casimir-force
  "Solves for whichever of force (N), area (m^2) or distance (m) is passed
  as 0, given the other two. Returns a single-entry map. Throws ex-info
  unless exactly one argument is 0 or any argument is negative."
  [force area distance]
  (when (not= 1 (count (filter zero? [force area distance])))
    (throw (ex-info "One and only one argument must be 0" {})))
  (when (< force 0)
    (throw (ex-info "Magnitude of force can not be negative" {:force force})))
  (when (< distance 0)
    (throw (ex-info "Distance can not be negative" {:distance distance})))
  (when (< area 0)
    (throw (ex-info "Area can not be negative" {:area area})))
  (let [k (* reduced-planck-constant speed-of-light Math/PI Math/PI)]
    (cond
      (zero? force)
      {:force (/ (* k area) (* 240 (Math/pow distance 4)))}
      (zero? area)
      {:area (/ (* 240 force (Math/pow distance 4)) k)}
      :else
      {:distance (Math/pow (/ (* k area) (* 240 force)) (/ 1 4))})))
