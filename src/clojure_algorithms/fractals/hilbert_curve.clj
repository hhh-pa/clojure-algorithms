(ns clojure-algorithms.fractals.hilbert-curve)

(defn rotate-pnts
  "Rotate a list of points by angle (a multiple of 90 degrees) by
  reordering: the start index shifts by angle/90 positions."
  [pnts angle]
  (let [start-index (mod (quot angle 90) 4)]
    (vec (concat (drop start-index pnts) (take start-index pnts)))))

(defn hilbert-curve
  "Points [x y] of the Hilbert curve of recursion level inside the square
  of side centered at center. Angle is the initial rotation in degrees."
  ([center level] (hilbert-curve center level 1 90))
  ([center level side angle]
   (let [[x y] center
         side (double side)
         angle (mod angle 360)
         pnts (rotate-pnts [[(+ x (/ side 4)) (+ y (/ side 4))]
                            [(- x (/ side 4)) (+ y (/ side 4))]
                            [(- x (/ side 4)) (- y (/ side 4))]
                            [(+ x (/ side 4)) (- y (/ side 4))]]
                           angle)]
     (if (= level 1)
       pnts
       (vec (concat (reverse (hilbert-curve (nth pnts 0) (dec level) (/ side 2) (+ angle 90)))
                    (hilbert-curve (nth pnts 1) (dec level) (/ side 2) angle)
                    (hilbert-curve (nth pnts 2) (dec level) (/ side 2) angle)
                    (reverse (hilbert-curve (nth pnts 3) (dec level) (/ side 2) (- angle 90)))))))))

;; Note: plot-hilbert-curve (matplotlib display) is intentionally not ported;
;; hilbert-curve returns the point data.
