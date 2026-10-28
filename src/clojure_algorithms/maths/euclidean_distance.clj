(ns clojure-algorithms.maths.euclidean-distance
  "Euclidean distance between the endpoints of two vectors.
A vector is any sequential of numbers.")

(defn- distance
  [vector-1 vector-2]
  (Math/sqrt (reduce + 0.0
                     (map (fn [v1 v2]
                            (let [d (- (double v1) (double v2))]
                              (* d d)))
                          vector-1 vector-2))))

(defn euclidean-distance
  "Calculate the distance between the two endpoints of two vectors.
A vector is defined as a sequential of numbers."
  [vector-1 vector-2]
  (distance vector-1 vector-2))

(defn euclidean-distance-no-np
  "Calculate the distance between the two endpoints of two vectors without numpy.
A vector is defined as a sequential of numbers."
  [vector-1 vector-2]
  (distance vector-1 vector-2))
