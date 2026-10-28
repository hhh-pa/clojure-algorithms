(ns clojure-algorithms.physics.shear-stress-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.physics.shear-stress :refer [shear-stress]]))

(deftest test-shear-stress
  (testing "known values"
    (is (= ["area" 4.0] (shear-stress 25 100 0)))
    (is (= ["stress" 8.0] (shear-stress 0 1600 200)))
    (is (= ["tangential_force" 1200000] (shear-stress 1000 0 1200))))
  (testing "invalid inputs throw"
    (is (thrown? clojure.lang.ExceptionInfo (shear-stress 0 0 5)))
    (is (thrown? clojure.lang.ExceptionInfo (shear-stress -1 100 0)))
    (is (thrown? clojure.lang.ExceptionInfo (shear-stress 25 -100 0)))))
