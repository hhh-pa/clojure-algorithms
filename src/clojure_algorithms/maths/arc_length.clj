(ns clojure-algorithms.maths.arc-length
  "Port of maths/arc_length.py" (:require [clojure-algorithms.runtime.python :refer :all]))

(declare arc-length)

(defn arc-length ">>> arc_length(45, 5)\n3.9269908169872414\n>>> arc_length(120, 15)\n31.415926535897928\n>>> arc_length(90, 10)\n15.707963267948966"
  [angle radius] (py-mul (py-mul (py-mul 2 Math/PI) radius) (py-div angle 360)))
