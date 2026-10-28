(ns clojure-algorithms.physics.potential-energy
  "Gravitational potential energy near Earth's surface: U = m*g*h.

  Reference: https://en.m.wikipedia.org/wiki/Gravitational_energy")

(def g
  "Acceleration due to gravity on Earth in m/s^2."
  9.80665)

(defn potential-energy
  "Returns the potential energy in Joules given mass (kg) and height (m)."
  [mass height]
  (cond
    (< mass 0)
    (throw (ex-info "The mass of a body cannot be negative" {:mass mass}))
    (< height 0)
    (throw (ex-info "The height above the ground cannot be negative"
                    {:height height}))
    :else (* mass g height)))
