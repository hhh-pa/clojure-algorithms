(ns clojure-algorithms.physics.rotational-partition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.rotational-partition
             :refer [rotation-partition-function]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-rotation-partition-function
  (testing "known values"
    (is (approx= (rotation-partition-function 1e-46 300) 5.9275 1e-4))
    (is (approx= (rotation-partition-function 2e-46 300) 11.855 1e-3)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (rotation-partition-function -2e-46 300)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (rotation-partition-function 1e-46 -300)))))
