(ns clojure-algorithms.maths.radians
  "Port of maths/radians.py" (:require [clojure-algorithms.runtime.python :refer :all]))

(declare radians)

(defn radians "Converts the given angle from degrees to radians\nhttps://en.wikipedia.org/wiki/Radian\n\n>>> radians(180)\n3.141592653589793\n>>> radians(92)\n1.6057029118347832\n>>> radians(274)\n4.782202150464463\n>>> radians(109.82)\n1.9167205845401725\n\n>>> from math import radians as math_radians\n>>> all(abs(radians(i) - math_radians(i)) <= 1e-8 for i in range(-2, 361))\nTrue"
  [degree] (py-div degree (py-div 180 Math/PI)))
