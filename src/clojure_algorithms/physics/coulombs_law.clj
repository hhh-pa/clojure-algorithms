(ns clojure-algorithms.physics.coulombs-law
  "Electrostatic force between two point charges: F = k*q1*q2/r^2.

  https://en.wikipedia.org/wiki/Coulomb%27s_law")

(def coulombs-constant
  "Coulomb's constant k = 1/(4*pi*eps0) in N m^2 C^-2."
  8.9875517923e9)

(defn coulombs-law
  "Returns the electrostatic force in Newtons given charges q1, q2 (C)
  and separation radius (m), rounded to 2 decimals."
  [q1 q2 radius]
  (when (<= radius 0)
    (throw (ex-info "The radius is always a positive number" {:radius radius})))
  (/ (double (Math/round (* (/ (* coulombs-constant q1 q2) (* radius radius)) 100)))
     100))
