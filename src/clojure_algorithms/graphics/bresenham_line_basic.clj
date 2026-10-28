(ns clojure-algorithms.graphics.bresenham-line-basic
  "Classic Bresenham line generation for slopes 0 <= m <= 1.
   Plotting (matplotlib in the Python original) is omitted.")

(defn classic-bresenham-line
  "Returns the raster points [[x y] ...] from p1 to p2."
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)]
    (loop [x x1 y y1 p (- (* 2 dy) dx) acc []]
      (if (> x x2)
        acc
        (if (neg? p)
          (recur (inc x) y (+ p (* 2 dy)) (conj acc [x y]))
          (recur (inc x) (inc y) (+ p (* 2 (- dy dx))) (conj acc [x y])))))))
