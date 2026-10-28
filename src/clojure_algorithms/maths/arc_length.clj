(ns clojure-algorithms.maths.arc-length
  "Port of maths/arc_length.py.")

(defn arc-length
  "Returns the arc length of a circle sector with the given angle
  (in degrees) and radius."
  [angle radius]
  (* 2 Math/PI radius (/ angle 360)))
