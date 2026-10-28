(ns clojure-algorithms.physics.relativistic-velocity-summation
  "Relativistic velocity addition: v2 = (v1 + v)/(1 + v1*v/c^2).

  https://en.wikipedia.org/wiki/Velocity-addition_formula")

(def c
  "Speed of light in vacuum in m/s."
  299792458.0)

(defn relativistic-velocity-summation
  "Returns the combined velocity of an object at object-velocity relative to
  a frame itself moving at frame-velocity (both in m/s)."
  [object-velocity frame-velocity]
  (when (or (> object-velocity c) (>= frame-velocity c)
            (< object-velocity (- c)) (<= frame-velocity (- c)))
    (throw (ex-info (str "Speeds must not exceed light speed, and "
                         "the frame speed must be lower than the light speed!")
                    {})))
  (/ (+ object-velocity frame-velocity)
     (+ 1 (/ (* object-velocity frame-velocity) (* c c)))))
