(ns clojure-algorithms.fractals.vicsek)

;; Pure-data port of the turtle-based original: instead of drawing,
;; the functions return geometry (cross polygons and fractal centers).

(defn cross-polygon
  "Vertices of the plus-shaped cross polygon centered at (x, y) with
  total extent length. The cross is the plus made of 5 squares of side
  length/3 (center plus four arms)."
  [x y length]
  (let [h (/ (double length) 2)   ; half extent
        t (/ (double length) 6)]  ; half arm thickness
    [[(- x t) (- y h)] [(+ x t) (- y h)]
     [(+ x t) (- y t)] [(+ x h) (- y t)]
     [(+ x h) (+ y t)] [(+ x t) (+ y t)]
     [(+ x t) (+ y h)] [(- x t) (+ y h)]
     [(- x t) (+ y t)] [(- x h) (+ y t)]
     [(- x h) (- y t)] [(- x t) (- y t)]]))

(defn fractal-centers
  "Lazy seq of [cx cy size] crosses forming the Vicsek fractal centered
  at (x, y) with extent length at recursion depth."
  [x y length depth]
  (if (zero? depth)
    (list [(double x) (double y) (double length)])
    (let [third (/ (double length) 3)]
      (mapcat (fn [[cx cy]] (fractal-centers cx cy third (dec depth)))
              [[x y] [(+ x third) y] [(- x third) y]
               [x (+ y third)] [x (- y third)]]))))

(defn vicsek-fractal
  "Cross polygons of the Vicsek fractal at (x, y) with extent length
  and recursion depth."
  [x y length depth]
  (mapv (fn [[cx cy size]] (cross-polygon cx cy size))
        (fractal-centers x y length depth)))
