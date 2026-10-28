(ns clojure-algorithms.scheduling.cpuschedulingalgorithms-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.scheduling.cpuschedulingalgorithms :as cpu]))

(deftest simulate-fcfs-test
  (testing "single process steps"
    (let [{:keys [steps]} (cpu/simulate-fcfs [{:pid "P1" :arrival 0 :burst 2}])]
      (is (= [[0 "P1" []] [1 "P1" []]] steps))))
  (testing "FCFS via simulate with stats"
    (let [{:keys [steps stats]} (cpu/simulate
                                 [{:pid "P1" :arrival 0 :burst 2}] "FCFS")]
      (is (= [[0 "P1" []] [1 "P1" []]] (take 2 steps)))
      (is (= [["P1" 0 2 2 2 0 0]] stats))))
  (testing "FCFS respects arrival order"
    (let [{:keys [stats]} (cpu/simulate [{:pid "P1" :arrival 1 :burst 2}
                                         {:pid "P2" :arrival 0 :burst 1}]
                                        "fcfs")]
      (is (= ["P2" "P1"] (mapv first stats))))))

(deftest simulate-sjf-test
  (testing "non-preemptive SJF runs the shortest ready job first"
    (let [{:keys [stats]} (cpu/simulate [{:pid "P1" :arrival 0 :burst 4}
                                         {:pid "P2" :arrival 0 :burst 2}]
                                        "SJF (Non-Preemptive)")]
      (is (= 2 (:completion (second (:processes
                                     (cpu/simulate-sjf-np
                                      [{:pid "P1" :arrival 0 :burst 4}
                                       {:pid "P2" :arrival 0 :burst 2}]))))))
      (is (= [["P1" 0 4 6 6 2 2] ["P2" 0 2 2 2 0 0]] stats))))
  (testing "preemptive SJF preempts on shorter arrival"
    (let [{:keys [timeline]} (cpu/simulate [{:pid "P1" :arrival 0 :burst 4}
                                            {:pid "P2" :arrival 1 :burst 1}]
                                           "SJF (Preemptive)")]
      (is (= "P2" (second (nth timeline 1)))))))

(deftest simulate-priority-test
  (testing "non-preemptive priority honors lower value first"
    (let [{:keys [stats]} (cpu/simulate [{:pid "P1" :arrival 0 :burst 2
                                          :priority 2}
                                         {:pid "P2" :arrival 0 :burst 2
                                          :priority 1}]
                                        "Priority (Non-Preemptive)")]
      (is (= 2 (nth (second stats) 3)))))
  (testing "preemptive priority preempts on higher priority arrival"
    (let [{:keys [timeline]} (cpu/simulate [{:pid "P1" :arrival 0 :burst 4
                                             :priority 2}
                                            {:pid "P2" :arrival 1 :burst 1
                                             :priority 1}]
                                           "Priority (Preemptive)")]
      (is (= "P2" (second (nth timeline 1)))))))

(deftest simulate-rr-test
  (testing "round robin interleaves with quantum 2"
    (let [{:keys [steps stats]} (cpu/simulate [{:pid "P1" :arrival 0 :burst 4}
                                               {:pid "P2" :arrival 0 :burst 2}]
                                              "Round Robin" 2)]
      (is (= [[0 "P1" ["P2"]] [1 "P1" ["P2"]]
              [2 "P2" ["P1"]] [3 "P2" ["P1"]]
              [4 "P1" []] [5 "P1" []]]
             steps))
      (is (= [["P1" 0 4 6 6 2 0] ["P2" 0 2 4 4 2 2]] stats)))))

(deftest simulate-errors-test
  (testing "unknown algorithm throws"
    (is (thrown? clojure.lang.ExceptionInfo
                 (cpu/simulate [{:pid "P1" :arrival 0 :burst 1}] "bogus")))))
