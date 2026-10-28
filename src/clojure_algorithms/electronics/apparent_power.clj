(ns clojure-algorithms.electronics.apparent-power
  "Apparent power in a single-phase AC circuit.

  Complex quantities are [real imag] pairs.
  Ported from TheAlgorithms/Python electronics/apparent_power.py.")

(defn- rect
  "Polar (magnitude, radians) to rectangular [re im]."
  [magnitude radians]
  [(* magnitude (Math/cos radians)) (* magnitude (Math/sin radians))])

(defn- complex-mul
  "Product of complex [re im] pairs."
  [[a b] [c d]]
  [(- (* a c) (* b d)) (+ (* a d) (* b c))])

(defn apparent-power
  "Apparent power from `voltage`/`current` magnitudes and their degree angles.
  Returns [real imag]."
  [voltage current voltage-angle current-angle]
  (complex-mul (rect voltage (Math/toRadians voltage-angle))
               (rect current (Math/toRadians current-angle))))
