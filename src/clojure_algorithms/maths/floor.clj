(ns clojure-algorithms.maths.floor
  "https://en.wikipedia.org/wiki/Floor_and_ceiling_functions")

(defn floor
  "Return the floor of x, the largest integer less than or equal to x."
  [x]
  (let [truncated (long x)]
    (if (>= (- x truncated) 0)
      truncated
      (dec truncated))))
