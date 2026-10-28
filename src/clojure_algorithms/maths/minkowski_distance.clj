(ns clojure-algorithms.maths.minkowski-distance
  "Minkowski distance between n-dimensional points.
  https://en.wikipedia.org/wiki/Minkowski_distance")

(defn minkowski-distance
  "Returns the Minkowski distance of the given `order` between the
  n-dimensional points `point-a` and `point-b`. Order 1 is the Manhattan
  distance; order 2 is the Euclidean distance."
  [point-a point-b order]
  (when (< order 1)
    (throw (ex-info "The order must be greater than or equal to 1." {:type :ValueError})))
  (when (not= (count point-a) (count point-b))
    (throw (ex-info "Both points must have the same dimension." {:type :ValueError})))
  (Math/pow (double (reduce + (map (fn [a b]
                                     (Math/pow (Math/abs (double (- a b)))
                                               (double order)))
                                   point-a point-b)))
            (/ 1.0 order)))
