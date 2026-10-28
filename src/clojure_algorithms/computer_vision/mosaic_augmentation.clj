(ns clojure-algorithms.computer-vision.mosaic-augmentation
  "Mosaic augmentation bounding-box math. cv2/numpy image IO is omitted;
  only the pure annotation transforms and helpers are ported.")

(defn mosaic-annos
  "Merges four annotation sets into mosaic coordinates.
  annos-by-quadrant holds [top-left top-right bottom-left bottom-right]
  boxes as [class xmin ymin xmax ymax]; scales are [scale-x scale-y]."
  [annos-by-quadrant [scale-x scale-y]]
  (vec (mapcat (fn [i boxes]
                 (mapv (fn [[cls xmin ymin xmax ymax]]
                         (case i
                           0 [cls (* xmin scale-x) (* ymin scale-y)
                              (* xmax scale-x) (* ymax scale-y)]
                           1 [cls (+ scale-x (* xmin (- 1 scale-x)))
                              (* ymin scale-y)
                              (+ scale-x (* xmax (- 1 scale-x)))
                              (* ymax scale-y)]
                           2 [cls (* xmin scale-x)
                              (+ scale-y (* ymin (- 1 scale-y)))
                              (* xmax scale-x)
                              (+ scale-y (* ymax (- 1 scale-y)))]
                           [cls (+ scale-x (* xmin (- 1 scale-x)))
                            (+ scale-y (* ymin (- 1 scale-y)))
                            (+ scale-x (* xmax (- 1 scale-x)))
                            (+ scale-y (* ymax (- 1 scale-y)))]))
                       boxes))
               (range 4) annos-by-quadrant)))

(defn filter-tiny
  "Drops boxes narrower or shorter than filter-scale."
  [annos filter-scale]
  (if (pos? filter-scale)
    (filterv (fn [[_ xmin ymin xmax ymax]]
               (and (< filter-scale (- xmax xmin))
                    (< filter-scale (- ymax ymin))))
             annos)
    (vec annos)))

(defn mosaic-quadrant-split
  "Computes the mosaic divid point for output [h w] and scales."
  [[out-h out-w] [scale-x scale-y]]
  [(int (* scale-y out-h)) (int (* scale-x out-w))])

(defn random-chars
  "Generates a random lowercase/digit string of length n."
  [n]
  (assert (> n 1) "The number of character should greater than 1")
  (apply str (repeatedly n #(rand-nth "abcdefghijklmnopqrstuvwxyz0123456789"))))
