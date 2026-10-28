(ns clojure-algorithms.physics.newtons-law-of-gravitation
  "Newton's law of universal gravitation: F = G*m1*m2/d^2.

  https://en.wikipedia.org/wiki/Newton%27s_law_of_universal_gravitation")

(def gravitational-constant
  "Gravitational constant G in m^3 kg^-1 s^-2."
  6.6743e-11)

(defn gravitational-law
  "Solves for whichever of force (N), mass-1 (kg), mass-2 (kg) or
  distance (m) is passed as 0, given the other three. Returns a
  single-entry map. Throws ex-info unless exactly one argument is 0
  or any argument is negative."
  [force mass-1 mass-2 distance]
  (when (not= 1 (count (filter zero? [force mass-1 mass-2 distance])))
    (throw (ex-info "One and only one argument must be 0" {})))
  (when (< force 0)
    (throw (ex-info "Gravitational force can not be negative" {:force force})))
  (when (< distance 0)
    (throw (ex-info "Distance can not be negative" {:distance distance})))
  (when (or (< mass-1 0) (< mass-2 0))
    (throw (ex-info "Mass can not be negative" {})))
  (cond
    (zero? force)
    {:force (/ (* gravitational-constant mass-1 mass-2) (* distance distance))}
    (zero? mass-1)
    {:mass-1 (/ (* force distance distance)
                (* gravitational-constant mass-2))}
    (zero? mass-2)
    {:mass-2 (/ (* force distance distance)
                (* gravitational-constant mass-1))}
    :else
    {:distance (Math/pow (/ (* gravitational-constant mass-1 mass-2) force)
                         0.5)}))
