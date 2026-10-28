(ns clojure-algorithms.physics.bragg-angle-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.bragg-angle :refer [bragg-angle]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-bragg-angle
  (testing "known values"
    (is (approx= (bragg-angle 2.2e-10 1 2.2e-10) 30.0 1e-9))
    (is (approx= (bragg-angle 5e-10 2 1e-10) 11.5 1e-9))
    (is (approx= (bragg-angle 4e-10 1 4e-10) 30.0 1e-9)))
  (testing "out-of-range sine throws"
    (is (thrown? clojure.lang.ExceptionInfo (bragg-angle 1e-10 2 3e-10)))))
