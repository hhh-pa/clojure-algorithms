(ns clojure-algorithms.electronics.resistor-equivalence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.electronics.resistor-equivalence :as re]))

(deftest parallel-test
  (testing "parallel resistance"
    (is (< (Math/abs (- (re/resistor-parallel [3.21389 2 3]) 0.8737571620498019))
           1e-15))
    (is (thrown? IllegalArgumentException (re/resistor-parallel [3.21389 2 -3])))
    (is (thrown? IllegalArgumentException (re/resistor-parallel [3.21389 2 0.0])))))

(deftest series-test
  (testing "series resistance"
    (is (< (Math/abs (- (re/resistor-series [3.21389 2 3]) 8.21389)) 1e-12))
    (is (thrown? IllegalArgumentException (re/resistor-series [3.21389 2 -3])))))
