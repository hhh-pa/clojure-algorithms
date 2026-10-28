(ns clojure-algorithms.physics.terminal-velocity
  "Terminal velocity of an object falling through a fluid.

  Vt = sqrt(2*m*g/(p*A*Cd))
  Reference: https://byjus.com/physics/derivation-of-terminal-velocity/")

(def g
  "Acceleration due to gravity on Earth in m/s^2."
  9.80665)

(defn terminal-velocity
  "Returns the terminal velocity in m/s given mass (kg), fluid density
  (kg/m^3), projected area (m^2) and drag coefficient."
  [mass density area drag-coefficient]
  (when (or (<= mass 0) (<= density 0) (<= area 0) (<= drag-coefficient 0))
    (throw (ex-info
            "mass, density, area and the drag coefficient all need to be positive"
            {})))
  (Math/pow (/ (* 2 mass g) (* density area drag-coefficient)) 0.5))
