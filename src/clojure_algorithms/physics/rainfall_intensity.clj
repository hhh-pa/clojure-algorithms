(ns clojure-algorithms.physics.rainfall-intensity
  "Rainfall intensity from the Sherman intensity-duration-frequency curve.

  I = k * T^a / (D + b)^c
  https://en.wikipedia.org/wiki/Intensity-duration-frequency_curve")

(defn rainfall-intensity
  "Returns the rainfall intensity in mm/h given Sherman coefficients k, a,
  b, c, the return period in years and the event duration in minutes."
  [coefficient-k coefficient-a coefficient-b coefficient-c return-period duration]
  (when (or (<= coefficient-k 0) (<= coefficient-a 0) (<= coefficient-b 0)
            (<= coefficient-c 0) (<= return-period 0) (<= duration 0))
    (throw (ex-info "All parameters must be positive." {})))
  (/ (* coefficient-k (Math/pow return-period coefficient-a))
     (Math/pow (+ duration coefficient-b) coefficient-c)))
