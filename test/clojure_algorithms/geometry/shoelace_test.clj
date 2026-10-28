(ns clojure-algorithms.geometry.shoelace-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.geometry.shoelace :refer [area-of-polygon]]))

(deftest test-area-of-polygon
  (is (< (Math/abs (- (area-of-polygon [1 3 7 4 8] [6 1 2 4 5]) 16.5)) 1e-9))
  (is (< (Math/abs (- (area-of-polygon [0 1 1 0] [0 0 1 1]) 1.0)) 1e-9))
  (is (< (Math/abs (- (area-of-polygon [0 4 4 0] [0 0 3 3]) 12.0)) 1e-9)))
