(ns clojure-algorithms.maths.floor
  "https://en.wikipedia.org/wiki/Floor_and_ceiling_functions"
  (:require [clojure-algorithms.runtime.python :refer :all] [clojure.string :as str] [clojure.set :as set]))

(declare floor)

(defn floor "Return the floor of x as an Integral.\n:param x: the number\n:return: the largest integer <= x.\n>>> import math\n>>> all(floor(n) == math.floor(n) for n\n...     in (1, -1, 0, -0, 1.1, -1.1, 1.0, -1.0, 1_000_000_000))\nTrue"
  [x] (if (py-truthy (py-ge (py-sub x (py-int x nil)) 0)) (py-int x nil) (py-sub (py-int x nil) 1)))
