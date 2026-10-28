(ns clojure-algorithms.maths.radians
  "Port of maths/radians.py")

(defn radians
  "Converts the given angle from degrees to radians.
  https://en.wikipedia.org/wiki/Radian"
  [degree]
  (/ degree (/ 180 Math/PI)))
