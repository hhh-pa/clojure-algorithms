(ns clojure-algorithms.physics.relativistic-velocity-summation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.relativistic-velocity-summation
             :refer [relativistic-velocity-summation]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-relativistic-velocity-summation
  (testing "known values"
    (is (approx= (relativistic-velocity-summation 200000000 200000000)
                 276805111.0636436 1e-6))
    (is (approx= (relativistic-velocity-summation 299792458 100000000)
                 299792458.0 1e-3)))
  (testing "superluminal speeds throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (relativistic-velocity-summation 100000000 299792458)))))
