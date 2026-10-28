(ns clojure-algorithms.physics.basic-orbital-capture
  "Radii of impact (orbital capture) for a projectile approaching a target body.

  R-capture = Rt * sqrt(1 + v-esc^2 / v^2), sigma = pi * R-capture^2.
  Does not account for the N-body problem.")

(def G
  "Gravitational constant in m^3 kg^-1 s^-2."
  6.6743e-11)

(def c
  "Speed of light in m/s."
  299792458.0)

(defn- round0
  "Rounds to a whole-number double, mirroring Python round(x, 0)."
  [x]
  (Math/floor (+ x 0.5)))

(defn capture-radii
  "Returns the capture radius in meters for a target body of given radius (m),
  mass (kg) and a projectile velocity (m/s)."
  [target-body-radius target-body-mass projectile-velocity]
  (cond
    (< target-body-mass 0)
    (throw (ex-info "Mass cannot be less than 0" {:mass target-body-mass}))
    (< target-body-radius 0)
    (throw (ex-info "Radius cannot be less than 0" {:radius target-body-radius}))
    (> projectile-velocity c)
    (throw (ex-info "Cannot go beyond speed of light" {:velocity projectile-velocity}))
    :else
    (let [escape-velocity-squared (/ (* 2 G target-body-mass) target-body-radius)
          capture-radius (* target-body-radius
                            (Math/sqrt (+ 1 (/ escape-velocity-squared
                                               (Math/pow projectile-velocity 2)))))]
      (round0 capture-radius))))

(defn capture-area
  "Returns the effective cross-sectional capture area in m^2 for a capture radius."
  [capture-radius]
  (if (< capture-radius 0)
    (throw (ex-info "Cannot have a capture radius less than 0"
                    {:radius capture-radius}))
    (round0 (* Math/PI (Math/pow capture-radius 2)))))
