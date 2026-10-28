(ns clojure-algorithms.physics.in-static-equilibrium
  "Static equilibrium check for a 2D system of forces.

  Pure-Clojure port of the numpy-based reference: forces and locations are
  plain vectors of [x y] pairs.")

(defn polar-force
  "Resolves a force given as (magnitude, angle) into [fx fy] components.
  Angle is in degrees unless radian-mode? is true."
  ([magnitude angle] (polar-force magnitude angle false))
  ([magnitude angle radian-mode?]
   (let [a (if radian-mode? angle (Math/toRadians angle))]
     [(* magnitude (Math/cos a)) (* magnitude (Math/sin a))])))

(defn in-static-equilibrium?
  "Returns true when the sum of moments about the z-axis is below eps.
  forces and location are seqs of [x y] pairs."
  ([forces location] (in-static-equilibrium? forces location 0.1))
  ([forces location eps]
   (let [moments (map (fn [[lx ly] [fx fy]] (- (* lx fy) (* ly fx)))
                      location forces)]
     (< (Math/abs (reduce + moments)) eps))))
