(ns clojure-algorithms.physics.translation-partition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.translation-partition
             :refer [translation-partition-function]]))

(defn- approx= [a b eps] (< (Math/abs (- (double a) (double b))) eps))

(deftest test-translation-partition-function
  (testing "known values"
    (is (approx= (translation-partition-function 2e-26 300 1e-3)
                 4.081816847078438e+28 1e20))
    (is (approx= (translation-partition-function 4e-26 300 1e-3)
                 1.1545121488522627e+29 1e21)))
  (testing "non-positive inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (translation-partition-function -4e-26 300 1e-3)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (translation-partition-function 4e-26 0 1e-3)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (translation-partition-function 4e-26 300 0)))))
