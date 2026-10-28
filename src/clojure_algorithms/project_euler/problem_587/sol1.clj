(ns clojure-algorithms.project-euler.problem-587.sol1
  "Project Euler Problem 587: concave triangle area.

  With n circles placed next to each other horizontally inside a rectangle,
  a diagonal from the bottom left to the top right cuts out a concave
  triangle from the L-section. Finds the least n for which the concave
  triangle occupies less than 0.1% of the L-section.")

(defn circle-bottom-arc-integral
  "Returns the integral of the circle bottom arc
  y = 1/2 - sqrt(1/4 - (x - 1/2)^2) evaluated at point."
  [point]
  (let [p (double point)]
    (/ (+ (* (- 1 (* 2 p)) (Math/sqrt (- p (* p p))))
          (* 2 p)
          (Math/asin (Math/sqrt (- 1 p))))
       4)))

(defn concave-triangle-area
  "Returns the area of the concave triangle for circles-number circles."
  [circles-number]
  (let [n (double circles-number)
        intersection-y (/ (- (+ n 1) (Math/sqrt (* 2 n)))
                          (* 2 (+ (* n n) 1)))
        intersection-x (* n intersection-y)
        triangle-area (/ (* intersection-x intersection-y) 2)
        concave-region-area (- (circle-bottom-arc-integral 0.5)
                               (circle-bottom-arc-integral intersection-x))]
    (+ triangle-area concave-region-area)))

(defn solution
  "Returns the least value of n for which the concave triangle occupies
  less than fraction of the L-section."
  ([] (solution (/ 1.0 1000)))
  ([fraction]
   (let [l-section-area (/ (- 1 (/ Math/PI 4)) 4)]
     (loop [n 1]
       (if (< (/ (concave-triangle-area n) l-section-area) fraction)
         n
         (recur (inc n)))))))
