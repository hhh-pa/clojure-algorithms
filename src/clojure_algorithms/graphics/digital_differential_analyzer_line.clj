(ns clojure-algorithms.graphics.digital-differential-analyzer-line
  "Digital Differential Analyzer (DDA) line generation.
   Plotting (matplotlib in the Python original) is omitted.")

(defn digital-differential-analyzer-line
  "Returns the raster points [[x y] ...] from p1 to p2, excluding p1.
   Uses round-half-to-even to match the Python original."
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)
        steps (max (Math/abs dx) (Math/abs dy))
        xi (/ (double dx) steps) yi (/ (double dy) steps)]
    (loop [i 0 x (double x1) y (double y1) acc []]
      (if (>= i steps)
        acc
        (let [nx (+ x xi) ny (+ y yi)]
          (recur (inc i) nx ny
                 (conj acc [(long (Math/rint nx)) (long (Math/rint ny))])))))))
