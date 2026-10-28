(ns clojure-algorithms.physics.snells-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.snells-law :refer [calculate-refraction-angle]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-calculate-refraction-angle
  (testing "known values"
    (is (approx= (calculate-refraction-angle 1.0 1.33 60.0) 40.63 1e-9))
    (is (approx= (calculate-refraction-angle 2.0 1.0 30.0) 90.0 1e-9)))
  (testing "impossible refraction throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (calculate-refraction-angle 1.33 1.0 60.0)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (calculate-refraction-angle -1.33 1.0 60.0)))))
