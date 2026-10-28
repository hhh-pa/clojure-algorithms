(ns clojure-algorithms.physics.light-aberration-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.light-aberration :refer [get-aberration-angle]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-get-aberration-angle
  (testing "known values"
    (is (approx= (get-aberration-angle 0.2 0.1) 0.18102 1e-9))
    (is (approx= (get-aberration-angle 0.2 0) 0.2 1e-9))
    (is (approx= (get-aberration-angle 0 0.2) 0.0 1e-9))
    (is (approx= (get-aberration-angle -1.5707963267948966 0.7) -0.7954 1e-9))))
