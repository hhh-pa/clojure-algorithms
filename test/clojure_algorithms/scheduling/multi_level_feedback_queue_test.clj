(ns clojure-algorithms.scheduling.multi-level-feedback-queue-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.multi-level-feedback-queue :as mlfq]))

(defn- fresh-state
  "Initial MLFQ state matching the Python doctests."
  []
  (mlfq/make-mlfq 3 [17 25]
                  [(mlfq/make-process "P1" 0 53)
                   (mlfq/make-process "P2" 0 17)
                   (mlfq/make-process "P3" 0 68)
                   (mlfq/make-process "P4" 0 24)]
                  0))

(defn- by-name
  "Finished processes of `state` ordered P1..P4."
  [state]
  (vec (sort-by :process-name (:finish-queue state))))

(deftest multi-level-feedback-queue-test
  (testing "finish order"
    (is (= ["P2" "P4" "P1" "P3"]
           (mlfq/calculate-sequence-of-finish-queue
            (mlfq/multi-level-feedback-queue (fresh-state))))))
  (testing "waiting, turnaround, and completion times"
    (let [final (mlfq/multi-level-feedback-queue (fresh-state))
          procs (by-name final)]
      (is (= [83 17 94 101] (mlfq/calculate-waiting-time procs)))
      (is (= [136 34 162 125] (mlfq/calculate-turnaround-time procs)))
      (is (= [136 34 162 125] (mlfq/calculate-completion-time procs))))))

(deftest round-robin-test
  (testing "one cycle with slice 17 finishes P2"
    (let [state (fresh-state)
          [finished ready state2]
          (mlfq/round-robin state (:ready-queue state) 17)]
      (is (= ["P2"] (mlfq/calculate-sequence-of-finish-queue state2)))
      (is (= [0] (mlfq/calculate-remaining-burst-time-of-processes finished)))
      (is (= [36 51 7]
             (mlfq/calculate-remaining-burst-time-of-processes ready)))
      (let [[finished2 ready2 state3] (mlfq/round-robin state2 ready 25)]
        (is (= [0 0] (mlfq/calculate-remaining-burst-time-of-processes
                      (:finish-queue state3))))
        (is (= [11 26] (mlfq/calculate-remaining-burst-time-of-processes
                        ready2)))))))

(deftest helpers-test
  (testing "waiting time update"
    (let [state (assoc (fresh-state) :current-time 10)
          p (assoc (mlfq/make-process "P1" 0 53) :stop-time 5)]
      (is (= 5 (:waiting-time (mlfq/update-waiting-time state p))))))
  (testing "FCFS finishes in arrival order"
    (let [state (fresh-state)
          [_finished state2]
          (mlfq/first-come-first-served state (:ready-queue state))]
      (is (= ["P1" "P2" "P3" "P4"]
             (mlfq/calculate-sequence-of-finish-queue state2))))))
