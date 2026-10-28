(ns clojure-algorithms.other.activity-selection-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.activity-selection :as a]))

(deftest max-activities-test
  (testing "classic example"
    (is (= [0 1 3 4]
           (a/max-activities [1 3 0 5 8 5] [2 4 6 7 9 9]))))
  (testing "empty input"
    (is (= [] (a/max-activities [] []))))
  (testing "single activity"
    (is (= [0] (a/max-activities [1] [2]))))
  (testing "all overlapping keeps first"
    (is (= [0] (a/max-activities [1 1 1] [5 5 5])))))
