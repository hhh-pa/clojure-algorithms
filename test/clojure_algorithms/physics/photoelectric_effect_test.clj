(ns clojure-algorithms.physics.photoelectric-effect-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.photoelectric-effect
             :refer [maximum-kinetic-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-maximum-kinetic-energy
  (testing "known values"
    (is (= 0 (maximum-kinetic-energy 1000000 2)))
    (is (= 0 (maximum-kinetic-energy 1000000 2 true)))
    (is (approx= (maximum-kinetic-energy 10000000000000000 2 true)
                 39.357000000000006 1e-9)))
  (testing "negative frequency throws"
    (is (thrown? clojure.lang.ExceptionInfo (maximum-kinetic-energy -9 20)))))
