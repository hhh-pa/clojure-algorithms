(ns clojure-algorithms.physics.hubble-parameter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.hubble-parameter :refer [hubble-parameter]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-hubble-parameter
  (testing "known value (LCDM at z=0 returns H0)"
    (is (approx= (hubble-parameter 68.3 1e-4 0.3 0.7 0) 68.3 1e-6)))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (hubble-parameter 68.3 1e-4 -0.3 0.7 1)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (hubble-parameter 68.3 1e-4 1.2 0.7 1)))))
