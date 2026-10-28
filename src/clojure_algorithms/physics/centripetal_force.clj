(ns clojure-algorithms.physics.centripetal-force
  "Centripetal force on an object in curvilinear motion: F = m*v^2/r.")

(defn centripetal
  "Returns the centripetal force in Newtons given mass (kg),
  velocity (m/s) and radius (m)."
  [mass velocity radius]
  (cond
    (< mass 0)
    (throw (ex-info "The mass of the body cannot be negative" {:mass mass}))
    (<= radius 0)
    (throw (ex-info "The radius is always a positive non zero integer"
                    {:radius radius}))
    :else
    (/ (* mass velocity velocity) radius)))
