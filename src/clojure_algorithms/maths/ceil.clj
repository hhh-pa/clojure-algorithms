(ns clojure-algorithms.maths.ceil
  "https://en.wikipedia.org/wiki/Floor_and_ceiling_functions")

(defn ceil
  "Return the ceiling of x, the smallest integer >= x."
  [x]
  (if (integer? x)
    x
    (let [t (long x)]
      (if (<= (- x t) 0) t (inc t)))))
