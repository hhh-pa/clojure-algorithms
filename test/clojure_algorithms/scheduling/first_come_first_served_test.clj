(ns clojure-algorithms.scheduling.first-come-first-served-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.first-come-first-served :as fcfs]))

(deftest calculate-waiting-times-test
  (testing "waiting times accumulate previous durations"
    (is (= [0 5 15] (fcfs/calculate-waiting-times [5 10 15])))
    (is (= [0 1 3 6 10] (fcfs/calculate-waiting-times [1 2 3 4 5])))
    (is (= [0 10] (fcfs/calculate-waiting-times [10 3])))
    (is (= [] (fcfs/calculate-waiting-times [])))))

(deftest calculate-turnaround-times-test
  (testing "turnaround is duration plus waiting"
    (is (= [5 15 30] (fcfs/calculate-turnaround-times [5 10 15] [0 5 15])))
    (is (= [1 3 6 10 15]
           (fcfs/calculate-turnaround-times [1 2 3 4 5] [0 1 3 6 10])))
    (is (= [10 13] (fcfs/calculate-turnaround-times [10 3] [0 10])))))

(deftest averages-test
  (testing "average turnaround and waiting times"
    (is (= 7.0 (fcfs/calculate-average-turnaround-time [0 5 16])))
    (is (= 6.5 (fcfs/calculate-average-turnaround-time [1 5 8 12])))
    (is (= 17.0 (fcfs/calculate-average-turnaround-time [10 24])))
    (is (= 7.0 (fcfs/calculate-average-waiting-time [0 5 16])))
    (is (= 6.5 (fcfs/calculate-average-waiting-time [1 5 8 12])))
    (is (= 17.0 (fcfs/calculate-average-waiting-time [10 24])))))
