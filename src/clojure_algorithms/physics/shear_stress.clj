(ns clojure-algorithms.physics.shear-stress
  "Shear stress relations: stress = tangential-force / area.

  https://en.wikipedia.org/wiki/Shear_stress")

(defn shear-stress
  "Solves for whichever of stress, tangential-force or area is passed as 0,
  given the other two. Returns a [name value] pair. Throws ex-info unless
  exactly one argument is 0 or any argument is negative."
  [stress tangential-force area]
  (when (not= 1 (count (filter zero? [stress tangential-force area])))
    (throw (ex-info "You cannot supply more or less than 2 values" {})))
  (when (< stress 0)
    (throw (ex-info "Stress cannot be negative" {})))
  (when (< tangential-force 0)
    (throw (ex-info "Tangential Force cannot be negative" {})))
  (when (< area 0)
    (throw (ex-info "Area cannot be negative" {})))
  (cond
    (zero? stress) ["stress" (double (/ tangential-force area))]
    (zero? tangential-force) ["tangential_force" (* stress area)]
    :else ["area" (double (/ tangential-force stress))]))
