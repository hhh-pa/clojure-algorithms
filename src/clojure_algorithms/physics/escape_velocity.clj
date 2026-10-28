(ns clojure-algorithms.physics.escape-velocity
  "Escape velocity from a celestial body: v = sqrt(2*G*M/R).

  https://en.wikipedia.org/wiki/Escape_velocity")

(def gravitational-constant
  "Gravitational constant in m^3 kg^-1 s^-2."
  6.67430e-11)

(defn escape-velocity
  "Returns the escape velocity in m/s given body mass (kg) and radius (m),
  rounded to 3 decimals. Throws ArithmeticException when radius is zero."
  [mass radius]
  (when (zero? radius)
    (throw (ArithmeticException. "Radius cannot be zero.")))
  (/ (double (Math/round (* (Math/sqrt (/ (* 2 gravitational-constant mass)
                                          radius))
                            1000)))
     1000))
