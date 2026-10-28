(ns clojure-algorithms.physics.speeds-of-gas-molecules-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.speeds-of-gas-molecules
             :refer [avg-speed-of-molecule mps-speed-of-molecule]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-avg-speed-of-molecule
  (testing "known values"
    (is (approx= (avg-speed-of-molecule 273 0.028) 454.3488755062257 1e-9))
    (is (approx= (avg-speed-of-molecule 300 0.032) 445.5257273433045 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (avg-speed-of-molecule -273 0.028)))
    (is (thrown? clojure.lang.ExceptionInfo (avg-speed-of-molecule 273 0)))))

(deftest test-mps-speed-of-molecule
  (testing "known values"
    (is (approx= (mps-speed-of-molecule 273 0.028) 402.65620702280023 1e-9))
    (is (approx= (mps-speed-of-molecule 300 0.032) 394.8368955535605 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (mps-speed-of-molecule -273 0.028)))
    (is (thrown? clojure.lang.ExceptionInfo (mps-speed-of-molecule 273 0)))))
