(ns clojure-algorithms.maths.line-intersection
  "Port of maths/line_intersection.py")

(defn determinant
  "Calculates the determinant of a 2x2 matrix:

| m00  m01 |
| m10  m11 |"
  [m00 m01 m10 m11]
  (- (* m00 m11) (* m10 m01)))

(defn line-coefficients
  "Computes the coefficients [A B C] of the line equation Ax + By + C = 0
from two points."
  [p1 p2]
  (if (= (nth p1 0) (nth p2 0))
    [1 0 (nth p1 0)]
    (let [a (/ (double (- (nth p2 1) (nth p1 1)))
               (double (- (nth p2 0) (nth p1 0))))
          b -1
          c (- (nth p2 1) (* a (nth p2 0)))]
      [a b c])))

(defn- within-bounds?
  "Checks that point [x y] lies within both segments' bounding boxes."
  [x y v1 v2 v1-prime v2-prime]
  (and (<= (min (nth v1 0) (nth v2 0)) x (max (nth v1 0) (nth v2 0)))
       (<= (min (nth v1-prime 0) (nth v2-prime 0)) x
           (max (nth v1-prime 0) (nth v2-prime 0)))
       (<= (min (nth v1 1) (nth v2 1)) y (max (nth v1 1) (nth v2 1)))
       (<= (min (nth v1-prime 1) (nth v2-prime 1)) y
           (max (nth v1-prime 1) (nth v2-prime 1)))))

(defn segment-intersection
  "Finds the intersection point of two line segments or lines, if it exists.
Treats the inputs as segments when as-segments is true (the default), or as
infinite lines when false. Returns [x y], or nil when there is no (unique)
intersection. Uses Cramer's rule:
https://en.wikipedia.org/wiki/Cramer%27s_rule"
  ([v1 v2 v1-prime v2-prime]
   (segment-intersection v1 v2 v1-prime v2-prime true))
  ([v1 v2 v1-prime v2-prime as-segments]
   (let [[a b c] (line-coefficients v1 v2)
         [a-prime b-prime c-prime] (line-coefficients v1-prime v2-prime)
         d (determinant a b a-prime b-prime)]
     (when-not (zero? d)
       (let [x (/ (double (determinant (- c) b (- c-prime) b-prime)) (double d))
             y (/ (double (determinant a (- c) a-prime (- c-prime))) (double d))]
         (when (or (not as-segments)
                   (within-bounds? x y v1 v2 v1-prime v2-prime))
           [x y]))))))
