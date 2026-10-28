(ns clojure-algorithms.physics.hookes-law-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.hookes-law :refer [hookes-law]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-hookes-law
  (testing "known values"
    (is (approx= (hookes-law 50 0.1) -5.0 1e-9))
    (is (approx= (hookes-law 100 0.5) -50.0 1e-9))
    (is (approx= (hookes-law 200 -0.2) 40.0 1e-9)))
  (testing "non-positive spring constant throws"
    (is (thrown? clojure.lang.ExceptionInfo (hookes-law 0 0.1)))
    (is (thrown? clojure.lang.ExceptionInfo (hookes-law -10 0.1)))))
