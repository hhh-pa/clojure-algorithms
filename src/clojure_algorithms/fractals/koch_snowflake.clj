(ns clojure-algorithms.fractals.koch-snowflake)

;; Pure-logic port of the NumPy original: 2D vectors are [x y] pairs.
;; Matplotlib plotting (plot) is intentionally not ported.

(def vector-1 [0 0])
(def vector-2 [0.5 0.8660254])
(def vector-3 [1 0])
(def initial-vectors [vector-1 vector-2 vector-3 vector-1])

(defn rotate
  "Standard rotation of a 2D vector by angle-in-degrees."
  [[x y] angle-in-degrees]
  (let [theta (Math/toRadians angle-in-degrees)
        c (Math/cos theta)
        s (Math/sin theta)]
    [(- (* c x) (* s y)) (+ (* s x) (* c y))]))

(defn iteration-step
  "Subdivide every segment: each pair of adjacent vectors gains three
  points, the middle one bent outward by a 60-degree rotation."
  [vectors]
  (let [step (fn [[[sx sy] [ex ex-y]]]
               (let [dx (- ex sx) dy (- ex-y sy)
                     [rx ry] (rotate [(/ dx 3) (/ dy 3)] 60)]
                 [[sx sy]
                  [(+ sx (/ dx 3)) (+ sy (/ dy 3))]
                  [(+ sx (/ dx 3) rx) (+ sy (/ dy 3) ry)]
                  [(+ sx (* dx (/ 2 3))) (+ sy (* dy (/ 2 3)))]]))]
    (vec (concat (mapcat step (partition 2 1 vectors)) [(last vectors)]))))

(defn iterate-snowflake
  "Apply iteration-step steps times to initial-vectors."
  [initial-vectors steps]
  (nth (iterate iteration-step initial-vectors) steps))
