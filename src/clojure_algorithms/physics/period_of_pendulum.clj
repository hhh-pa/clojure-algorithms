(ns clojure-algorithms.physics.period-of-pendulum
  "Period of a simple pendulum (small amplitudes): T = 2*pi*sqrt(L/g).

  Reference: https://byjus.com/jee/simple-pendulum/")

(def g
  "Acceleration due to gravity on Earth in m/s^2."
  9.80665)

(defn period-of-pendulum
  "Returns the oscillation period in seconds given the string length in meters."
  [length]
  (when (< length 0)
    (throw (ex-info "The length should be non-negative" {:length length})))
  (* 2 Math/PI (Math/pow (/ length g) 0.5)))
