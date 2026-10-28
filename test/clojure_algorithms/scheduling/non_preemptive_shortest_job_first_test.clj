(ns clojure-algorithms.scheduling.non-preemptive-shortest-job-first-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.non-preemptive-shortest-job-first :as sjf]))

(deftest calculate-waitingtime-test
  (testing "non-preemptive SJF waiting times"
    (is (= [0 9 13] (sjf/calculate-waitingtime [0 1 2] [10 5 8] 3)))
    (is (= [0 7 4 1] (sjf/calculate-waitingtime [1 2 2 4] [4 6 3 1] 4)))
    (is (= [12 0 2] (sjf/calculate-waitingtime [0 0 0] [12 2 10] 3)))))

(deftest calculate-turnaroundtime-test
  (testing "turnaround is burst plus waiting"
    (is (= [0 11 17] (sjf/calculate-turnaroundtime [0 1 2] 3 [0 10 15])))
    (is (= [2 10 7 8] (sjf/calculate-turnaroundtime [1 2 2 4] 4 [1 8 5 4])))
    (is (= [12 0 2] (sjf/calculate-turnaroundtime [0 0 0] 3 [12 0 2])))))
