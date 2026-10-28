(ns clojure-algorithms.physics.altitude-pressure-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.altitude-pressure :refer [get-altitude-at-pressure]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-get-altitude-at-pressure
  (testing "known values"
    (is (approx= (get-altitude-at-pressure 100000) 105.47836610778828 1e-6))
    (is (approx= (get-altitude-at-pressure 101325) 0.0 1e-9))
    (is (approx= (get-altitude-at-pressure 80000) 1855.873388064995 1e-6)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (get-altitude-at-pressure 201325)))
    (is (thrown? clojure.lang.ExceptionInfo (get-altitude-at-pressure -80000)))))
