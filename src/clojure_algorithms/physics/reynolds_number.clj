(ns clojure-algorithms.physics.reynolds-number
  "Reynolds number for flow through a pipe: R = p*V*D/mu.

  Reference: https://byjus.com/physics/reynolds-number/")

(defn reynolds-number
  "Returns the dimensionless Reynolds number given fluid density (kg/m^3),
  flow velocity (m/s), pipe diameter (m) and viscosity (N s/m^2)."
  [density velocity diameter viscosity]
  (when (or (<= density 0) (<= diameter 0) (<= viscosity 0))
    (throw (ex-info
            "please ensure that density, diameter and viscosity are positive"
            {})))
  (/ (* density (Math/abs (double velocity)) diameter) viscosity))
