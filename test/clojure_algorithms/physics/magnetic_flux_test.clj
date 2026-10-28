(ns clojure-algorithms.physics.magnetic-flux-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.magnetic-flux :refer [magnetic-flux]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-magnetic-flux
  (testing "known values"
    (is (approx= (magnetic-flux 50.0 2 0.0) 100.0 1e-9))
    (is (approx= (magnetic-flux 50 2 60.0) 50.0 1e-9))
    (is (approx= (magnetic-flux 0.5 4.0 90.0) 0.0 1e-9))
    (is (approx= (magnetic-flux 1 2.0 180.0) -2.0 1e-9)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (magnetic-flux -1.0 2.0 30.0)))
    (is (thrown? clojure.lang.ExceptionInfo (magnetic-flux 1.0 "a" 30.0)))
    (is (thrown? clojure.lang.ExceptionInfo (magnetic-flux 1.0 -2.0 30.0)))
    (is (thrown? clojure.lang.ExceptionInfo (magnetic-flux 10 10 -10)))))
