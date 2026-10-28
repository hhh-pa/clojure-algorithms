(ns clojure-algorithms.geodesy.haversine-distance-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geodesy.haversine-distance :refer [haversine-distance]]))

(defn- approx? [a b tol] (< (Math/abs (- a b)) tol))

(deftest test-haversine-distance
  (is (approx? (haversine-distance 37.774856 -122.424227 37.864742 -119.537521)
               253748 1.0))
  (is (approx? (haversine-distance 40.712776 -74.005974 34.052235 -118.243683)
               3935746 1.0))
  (is (approx? (haversine-distance 51.507351 -0.127758 48.856614 2.352222)
               343549 1.0))
  (is (zero? (haversine-distance 0 0 0 0)))
  (is (approx? (haversine-distance 0 0 0 90) 10007543 10008)))
