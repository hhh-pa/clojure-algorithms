(ns clojure-algorithms.scheduling.shortest-job-first-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.shortest-job-first :as sjf]))

(deftest calculate-waitingtime-test
  (testing "preemptive SJF waiting times"
    (is (= [0 3 5 0] (sjf/calculate-waitingtime [1 2 3 4] [3 3 5 1] 4)))
    (is (= [0 2 0] (sjf/calculate-waitingtime [1 2 3] [2 5 1] 3)))
    (is (= [1 0] (sjf/calculate-waitingtime [2 3] [5 1] 2)))))

(deftest calculate-turnaroundtime-test
  (testing "turnaround is burst plus waiting"
    (is (= [3 6 10 1]
           (sjf/calculate-turnaroundtime [3 3 5 1] 4 [0 3 5 0])))
    (is (= [3 6] (sjf/calculate-turnaroundtime [3 3] 2 [0 3])))
    (is (= [9 10 4] (sjf/calculate-turnaroundtime [8 10 1] 3 [1 0 3])))))

(deftest calculate-average-times-test
  (testing "averages returned as a map"
    (is (= {:average-waiting 2.0 :average-turnaround 5.0}
           (sjf/calculate-average-times [0 3 5 0] [3 6 10 1] 4)))
    (is (= {:average-waiting 2.5 :average-turnaround 4.5}
           (sjf/calculate-average-times [2 3] [3 6] 2)))))
