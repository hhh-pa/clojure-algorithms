(ns clojure-algorithms.physics.orbital-transfer-work-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.orbital-transfer-work
             :refer [orbital-transfer-work]]))

(deftest test-orbital-transfer-work
  (testing "known values"
    (is (= "2.811e+09" (orbital-transfer-work 5.972e24 1000 6.371e6 7e6)))
    (is (= "-1.405e+09" (orbital-transfer-work 5.972e24 500 7e6 6.371e6)))
    (is (= "1.514e+11" (orbital-transfer-work 1.989e30 1000 1.5e11 2.28e11))))
  (testing "non-positive radii throw"
    (is (thrown? clojure.lang.ExceptionInfo
                 (orbital-transfer-work 5.972e24 1000 0 7e6)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (orbital-transfer-work 5.972e24 1000 6.371e6 -1)))))
