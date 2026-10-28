(ns clojure-algorithms.fractals.sierpinski-triangle)

;; The turtle drawing calls are replaced by pure data: triangle returns
;; the sequence of drawn triangles as vertex triples.

(defn get-mid
  "Midpoint of points p1 and p2."
  [[x1 y1] [x2 y2]]
  [(/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)])

(defn triangle
  "Lazy seq of drawn triangles (each [v1 v2 v3]) for a Sierpinski
  triangle with the given vertices at recursion depth."
  [vertex1 vertex2 vertex3 depth]
  (lazy-seq
   (cons [vertex1 vertex2 vertex3]
         (when (pos? depth)
           (concat (triangle vertex1 (get-mid vertex1 vertex2) (get-mid vertex1 vertex3) (dec depth))
                   (triangle vertex2 (get-mid vertex1 vertex2) (get-mid vertex2 vertex3) (dec depth))
                   (triangle vertex3 (get-mid vertex3 vertex2) (get-mid vertex1 vertex3) (dec depth)))))))
