(ns clojure-algorithms.physics.mass-energy-equivalence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.mass-energy-equivalence
             :refer [energy-from-mass mass-from-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-energy-from-mass
  (testing "known values"
    (is (approx= (energy-from-mass 124.56) 1.11948945063458e+19 1e5))
    (is (approx= (energy-from-mass 320) 2.8760165719578165e+19 1e5))
    (is (approx= (energy-from-mass 0) 0.0 1e-9)))
  (testing "negative mass throws"
    (is (thrown? clojure.lang.ExceptionInfo (energy-from-mass -967.9)))))

(deftest test-mass-from-energy
  (testing "known values"
    (is (approx= (mass-from-energy 124.56) 1.3859169098203872e-15 1e-28))
    (is (approx= (mass-from-energy 320) 3.560480179371579e-15 1e-28))
    (is (approx= (mass-from-energy 0) 0.0 1e-9)))
  (testing "negative energy throws"
    (is (thrown? clojure.lang.ExceptionInfo (mass-from-energy -967.9)))))
