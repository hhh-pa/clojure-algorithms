(ns clojure-algorithms.physics.relativistic-kinetic-energy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.relativistic-kinetic-energy
             :refer [relativistic-kinetic-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-relativistic-kinetic-energy
  (testing "known values"
    (is (approx= (relativistic-kinetic-energy 10 10) 598.6912157608277 1e-6))
    (is (approx= (relativistic-kinetic-energy 40 200000) 800000266962.5057 1e-3))
    (is (approx= (relativistic-kinetic-energy 50 100000) 250000021062.11475 1e-3))
    (is (approx= (relativistic-kinetic-energy 0 0) 0.0 1e-9))
    (is (approx= (relativistic-kinetic-energy 100 0) 0.0 1e-9)))
  (testing "negative mass throws"
    (is (thrown? clojure.lang.ExceptionInfo (relativistic-kinetic-energy -1 10)))))
