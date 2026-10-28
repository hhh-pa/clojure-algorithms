(ns clojure-algorithms.geometry.triangle
  "Triangle centres (circumcenter, incenter, orthocenter) on a 2D grid.")

(defrecord Point [x y])
(defrecord Triangle [v1 v2 v3])

(defn make-point
  "Builds a Point, validating numeric coordinates."
  ([] (->Point 0.0 0.0))
  ([x y]
   (when-not (number? x)
     (throw (IllegalArgumentException. "x must be an int or float numeric value")))
   (when-not (number? y)
     (throw (IllegalArgumentException. "y must be an int or float numeric value")))
   (->Point x y)))

(defn- collinear? [v1 v2 v3]
  (zero? (- (* (- (:x v2) (:x v1)) (- (:y v3) (:y v1)))
            (* (- (:y v2) (:y v1)) (- (:x v3) (:x v1))))))

(defn make-triangle
  "Builds a Triangle from three unique, non-collinear Points."
  [v1 v2 v3]
  (doseq [v [v1 v2 v3]]
    (when-not (instance? Point v)
      (throw (IllegalArgumentException. "All 3 arguments should be Point Objects"))))
  (when (or (= v1 v2) (= v1 v3) (= v2 v3))
    (throw (IllegalArgumentException. "All 3 arguments should be unique")))
  (when (collinear? v1 v2 v3)
    (throw (IllegalArgumentException. "One or more arguments are redundant")))
  (->Triangle v1 v2 v3))

(defn- det3
  "Determinant of a 3x3 matrix given as 9 row-major values."
  [a b c d e f g h i]
  (- (+ (* a e i) (* b f g) (* c d h)) (* c e g) (* a f h) (* b d i)))

(defn circumcenter
  "Circumcenter of the triangle (determinant method)."
  [{:keys [v1 v2 v3]}]
  (let [s (fn [v] (+ (* (:x v) (:x v)) (* (:y v) (:y v))))
        a (det3 (:x v1) (:y v1) 1 (:x v2) (:y v2) 1 (:x v3) (:y v3) 1)
        bx (- (det3 (s v1) (:y v1) 1 (s v2) (:y v2) 1 (s v3) (:y v3) 1))
        by (det3 (s v1) (:x v1) 1 (s v2) (:x v2) 1 (s v3) (:x v3) 1)]
    (->Point (/ (- bx) (* 2 a)) (/ (- by) (* 2 a)))))

(defn incenter
  "Incenter of the triangle (side-weighted vertex average)."
  [{:keys [v1 v2 v3]}]
  (let [d (fn [p q] (Math/hypot (- (:x p) (:x q)) (- (:y p) (:y q))))
        a (d v2 v3) b (d v1 v3) c (d v1 v2)
        s (+ a b c)]
    (->Point (/ (+ (* a (:x v1)) (* b (:x v2)) (* c (:x v3))) s)
             (/ (+ (* a (:y v1)) (* b (:y v2)) (* c (:y v3))) s))))

(defn- solve-2x2
  "Solves [[m00 m01] [m10 m11]] x = [b0 b1] via Cramer's rule."
  [m00 m01 m10 m11 b0 b1]
  (let [det (- (* m00 m11) (* m01 m10))]
    [(/ (- (* b0 m11) (* m01 b1)) det)
     (/ (- (* m00 b1) (* b0 m10)) det)]))

(defn orthocenter
  "Orthocenter of the triangle (altitude intersection)."
  [{:keys [v1 v2 v3]}]
  (let [inv-m1 (/ -1 (/ (- (:y v3) (:y v1)) (- (:x v3) (:x v1))))
        inv-m2 (/ -1 (/ (- (:y v3) (:y v2)) (- (:x v3) (:x v2))))
        [ox oy] (solve-2x2 inv-m2 -1 inv-m1 -1
                           (- (* inv-m2 (:x v1)) (:y v1))
                           (- (* inv-m1 (:x v2)) (:y v2)))]
    (->Point ox oy)))
