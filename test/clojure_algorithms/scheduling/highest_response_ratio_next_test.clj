(ns clojure-algorithms.scheduling.highest-response-ratio-next-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.highest-response-ratio-next :as hrrn]))

(deftest calculate-turn-arround-time-test
  (testing "HRRN turnaround times"
    (is (= [2 4 7]
           (hrrn/calculate-turn-arround-time ["A" "B" "C"] [3 5 8] [2 4 6] 3)))
    (is (= [3 6 11]
           (hrrn/calculate-turn-arround-time ["A" "B" "C"] [0 2 4] [3 5 7] 3)))))

(deftest calculate-waiting-time-test
  (testing "waiting is turnaround minus burst"
    (is (= [0 0 1]
           (hrrn/calculate-waiting-time ["A" "B" "C"] [2 4 7] [2 4 6] 3)))
    (is (= [0 1 4]
           (hrrn/calculate-waiting-time ["A" "B" "C"] [3 6 11] [3 5 7] 3)))))
