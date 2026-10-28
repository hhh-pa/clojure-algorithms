(ns clojure-algorithms.geodesy.radar-target-calculation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geodesy.radar-target-calculation
             :refer [geodetic-to-ecef ecef-to-geodetic enu-to-ecef
                     calculate-target-coordinates]]))

(defn- approx-vec? [a b tol]
  (every? true? (map #(< (Math/abs (- %1 %2)) tol) a b)))

(deftest test-geodetic-ecef-roundtrip
  (is (approx-vec? (geodetic-to-ecef 0.0 0.0 0.0) [6378137.0 0.0 0.0] 0.01))
  (is (approx-vec? (geodetic-to-ecef 90.0 0.0 0.0) [0.0 0.0 6356752.31] 0.01))
  (is (approx-vec? (ecef-to-geodetic 6378137.0 0.0 0.0) [0.0 0.0 0.0] 0.01))
  (is (approx-vec? (ecef-to-geodetic 0.0 0.0 6356752.314245)
                   [90.0 0.0 0.0] 0.01)))

(deftest test-enu-and-target
  (is (approx-vec? (enu-to-ecef 100.0 200.0 50.0 0.0 0.0)
                   [50.0 100.0 200.0] 0.01))
  (let [[lat lon alt] (calculate-target-coordinates 0.0 0.0 0.0 90.0 111319.5)]
    (is (< (Math/abs (- lat 0.0)) 0.1))
    (is (< (Math/abs (- lon 1.0)) 0.1))
    (is (< (Math/abs (- alt 971.4)) 1.0))))
