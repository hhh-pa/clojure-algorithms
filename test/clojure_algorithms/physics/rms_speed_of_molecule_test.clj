(ns clojure-algorithms.physics.rms-speed-of-molecule-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.rms-speed-of-molecule :refer [rms-speed-of-molecule]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-rms-speed-of-molecule
  (testing "known values"
    (is (approx= (rms-speed-of-molecule 100 2) 35.315279554323226 1e-9))
    (is (approx= (rms-speed-of-molecule 273 12) 23.821458421977443 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (rms-speed-of-molecule -1 2)))
    (is (thrown? clojure.lang.ExceptionInfo (rms-speed-of-molecule 100 0)))
    (is (thrown? clojure.lang.ExceptionInfo (rms-speed-of-molecule 100 -2)))))
