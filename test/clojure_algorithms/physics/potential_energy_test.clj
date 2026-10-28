(ns clojure-algorithms.physics.potential-energy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.potential-energy :refer [potential-energy]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-potential-energy
  (testing "known values"
    (is (approx= (potential-energy 10 10) 980.665 1e-9))
    (is (approx= (potential-energy 0 5) 0.0 1e-9))
    (is (approx= (potential-energy 8 0) 0.0 1e-9))
    (is (approx= (potential-energy 10 5) 490.3325 1e-9))
    (is (approx= (potential-energy 0 0) 0.0 1e-9))
    (is (approx= (potential-energy 2 8) 156.9064 1e-9))
    (is (approx= (potential-energy 20 100) 19613.3 1e-9)))
  (testing "negative inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (potential-energy -1 5)))
    (is (thrown? clojure.lang.ExceptionInfo (potential-energy 5 -1)))))
