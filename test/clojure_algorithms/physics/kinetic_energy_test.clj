(ns clojure-algorithms.physics.kinetic-energy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.kinetic-energy :refer [kinetic-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-kinetic-energy
  (testing "known values"
    (is (approx= (kinetic-energy 10 10) 500.0 1e-9))
    (is (approx= (kinetic-energy 0 10) 0.0 1e-9))
    (is (approx= (kinetic-energy 10 0) 0.0 1e-9))
    (is (approx= (kinetic-energy 20 -20) 4000.0 1e-9))
    (is (approx= (kinetic-energy 0 0) 0.0 1e-9))
    (is (approx= (kinetic-energy 2 2) 4.0 1e-9))
    (is (approx= (kinetic-energy 100 100) 500000.0 1e-9)))
  (testing "negative mass throws"
    (is (thrown? clojure.lang.ExceptionInfo (kinetic-energy -1 10)))))
