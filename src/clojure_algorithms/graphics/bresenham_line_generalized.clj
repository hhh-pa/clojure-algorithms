(ns clojure-algorithms.graphics.bresenham-line-generalized
  "Generalized Bresenham line generation for all slopes and directions.
   Plotting (matplotlib in the Python original) is omitted.")

(defn generalized-bresenham-line
  "Returns the raster points [[x y] ...] from p1 to p2."
  [[x1 y1] [x2 y2]]
  (let [dx (Math/abs (- x2 x1))
        dy (Math/abs (- y2 y1))
        sx (if (< x1 x2) 1 -1)
        sy (if (< y1 y2) 1 -1)]
    (loop [x x1 y y1 err (- dx dy) acc []]
      (let [acc (conj acc [x y])]
        (if (and (= x x2) (= y y2))
          acc
          (let [e2 (* 2 err)
                [err x] (if (> e2 (- dy)) [(- err dy) (+ x sx)] [err x])
                [err y] (if (< e2 dx) [(+ err dx) (+ y sy)] [err y])]
            (recur x y err acc)))))))
