(ns clojure-algorithms.other.graham-scan-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.graham-scan :as g]))

(deftest angle-comparer-test
  (is (= 45.0 (g/angle-comparer [1 1] 0 0))))

(deftest check-direction-test
  (is (= :straight (g/check-direction [1 1] [2 2] [3 3])))
  (is (= :left (g/check-direction [60 1] [-50 199] [30 2])))
  (is (= :right (g/check-direction [0 0] [5 5] [10 0]))))

(deftest graham-scan-test
  (testing "doctest examples"
    (is (= [[0 0] [7 0] [9 6] [5 5] [1 4]]
           (g/graham-scan [[9 6] [3 1] [0 0] [5 5] [5 2] [7 0] [3 3] [1 4]])))
    (is (= [[0 0] [1 0] [1 1] [0 1]]
           (g/graham-scan [[0 0] [1 0] [1 1] [0 1]]))))
  (testing "too few points throws"
    (is (thrown? IllegalArgumentException (g/graham-scan [[0 0] [1 1]])))))
