(ns clojure-algorithms.maths.pythagoras
  "Uses Pythagoras theorem to calculate the distance between two points in space.")

(defrecord Point [x y z])

(defn make-Point
  "Build a Point in 3D space."
  [x y z]
  (->Point x y z))

(defn Point-repr
  "String representation of a Point."
  [self]
  (str "Point(" (:x self) ", " (:y self) ", " (:z self) ")"))

(defn distance
  "Euclidean distance between points a and b."
  [a b]
  (let [dx (- (:x b) (:x a))
        dy (- (:y b) (:y a))
        dz (- (:z b) (:z a))]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defmethod print-method Point [o w] (.write ^java.io.Writer w (Point-repr o)))
