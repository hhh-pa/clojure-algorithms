(ns clojure-algorithms.maths.ceil
  "https://en.wikipedia.org/wiki/Floor_and_ceiling_functions"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare ceil)

(defn ceil "Return the ceiling of x as an Integral.\n\n:param x: the number\n:return: the smallest integer >= x.\n\n>>> import math\n>>> all(ceil(n) == math.ceil(n) for n\n...     in (1, -1, 0, -0, 1.1, -1.1, 1.0, -1.0, 1_000_000_000))\nTrue"
  [x] (if (py-truthy (py-le (py-sub x (py-int x nil)) 0)) (py-int x nil) (py-add (py-int x nil) 1)))
