(ns clojure-algorithms.physics.newtons-second-law-of-motion
  "Newton's second law of motion: F = m*a.

  https://www.physicsclassroom.com/class/newtlaws/Lesson-3/Newton-s-Second-Law")

(defn newtons-second-law-of-motion
  "Returns the force in Newtons given mass (kg) and acceleration (m/s^2).
  Returns -0.0 when the inputs cannot be multiplied, mirroring the reference."
  [mass acceleration]
  (try
    (* mass acceleration)
    (catch Exception _ -0.0)))
