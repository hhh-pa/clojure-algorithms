(ns clojure-algorithms.scheduling.job-sequence-with-deadline-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.job-sequence-with-deadline :as jsd]))

(deftest max-tasks-test
  (testing "greedy max-reward selection"
    (is (= [2 0] (jsd/max-tasks [[4 20] [1 10] [1 40] [1 30]])))
    (is (= [3 2] (jsd/max-tasks [[1 10] [2 20] [3 30] [2 40]])))
    (is (= [0] (jsd/max-tasks [[9 10]]))))
  (testing "no schedulable tasks"
    (is (= [] (jsd/max-tasks [[-9 10]])))
    (is (= [] (jsd/max-tasks [])))
    (is (= [] (jsd/max-tasks [[0 10] [0 20] [0 30] [0 40]])))
    (is (= [] (jsd/max-tasks [[-1 10] [-2 20] [-3 30] [-4 40]])))))
