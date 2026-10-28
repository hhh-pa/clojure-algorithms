(ns clojure-algorithms.scheduling.round-robin-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.round-robin :as rr]))

(deftest calculate-waiting-times-test
  (testing "round robin waiting times with default quantum 2"
    (is (= [13 10 13] (rr/calculate-waiting-times [10 5 8])))
    (is (= [5 8 9 6] (rr/calculate-waiting-times [4 6 3 1])))
    (is (= [12 2 12] (rr/calculate-waiting-times [12 2 10]))))
  (testing "explicit quantum"
    (is (= [13 10 13] (rr/calculate-waiting-times [10 5 8] 2)))))

(deftest calculate-turn-arround-times-test
  (testing "turnaround is burst plus waiting"
    (is (= [1 3 6] (rr/calculate-turn-arround-times [1 2 3 4] [0 1 3])))
    (is (= [20 9 18] (rr/calculate-turn-arround-times [10 3 7] [10 6 11])))))
