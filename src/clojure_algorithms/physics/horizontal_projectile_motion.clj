(ns clojure-algorithms.physics.horizontal-projectile-motion
  "Horizontal projectile motion: range, max height and total time.

  https://en.wikipedia.org/wiki/Projectile_motion")

(def g
  "Acceleration due to gravity on Earth in m/s^2."
  9.80665)

(defn- check-args
  "Validates initial velocity and angle. Throws on invalid input."
  [init-velocity angle]
  (when (not (number? init-velocity))
    (throw (ex-info "Invalid velocity. Should be an integer or float." {})))
  (when (not (number? angle))
    (throw (ex-info "Invalid angle. Should be an integer or float." {})))
  (when (or (> angle 90) (< angle 1))
    (throw (ex-info "Invalid angle. Range is 1-90 degrees." {:angle angle})))
  (when (< init-velocity 0)
    (throw (ex-info "Invalid velocity. Should be a positive number."
                    {:velocity init-velocity}))))

(defn- round2 [x] (/ (double (Math/round (* x 100))) 100))

(defn horizontal-distance
  "Returns the horizontal range in meters: v0^2 * sin(2*alpha) / g."
  [init-velocity angle]
  (check-args init-velocity angle)
  (round2 (/ (* init-velocity init-velocity
                (Math/sin (Math/toRadians (* 2 angle))))
             g)))

(defn max-height
  "Returns the maximum height in meters: v0^2 * sin^2(alpha) / (2*g)."
  [init-velocity angle]
  (check-args init-velocity angle)
  (let [s (Math/sin (Math/toRadians angle))]
    (round2 (/ (* init-velocity init-velocity s s) (* 2 g)))))

(defn total-time
  "Returns the total flight time in seconds: 2*v0*sin(alpha)/g."
  [init-velocity angle]
  (check-args init-velocity angle)
  (round2 (/ (* 2 init-velocity (Math/sin (Math/toRadians angle))) g)))
