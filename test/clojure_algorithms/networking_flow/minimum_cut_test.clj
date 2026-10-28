(ns clojure-algorithms.networking-flow.minimum-cut-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.networking-flow.minimum-cut :as mc]))

(def classic
  [[0 16 13 0 0 0]
   [0 0 10 12 0 0]
   [0 4 0 0 14 0]
   [0 0 9 0 0 20]
   [0 0 0 7 0 4]
   [0 0 0 0 0 0]])

(deftest bfs-test
  (testing "sink reachable in classic graph"
    (is (true? (:found? (mc/bfs classic 0 5)))))
  (testing "sink unreachable in empty graph"
    (is (false? (:found? (mc/bfs [[0 0] [0 0]] 0 1))))))

(deftest mincut-test
  (testing "classic 6-vertex example"
    (is (= [[1 3] [4 3] [4 5]] (mc/mincut classic 0 5))))
  (testing "cut capacities sum to max flow (23)"
    (let [cut (mc/mincut classic 0 5)]
      (is (= 23 (reduce + 0 (map (fn [[u v]] (get-in classic [u v])) cut))))))
  (testing "single saturated edge is its own minimum cut"
    (is (= [[0 1]] (mc/mincut [[0 7] [0 0]] 0 1))))
  (testing "disconnected graph has an empty cut"
    (is (= [] (mc/mincut [[0 0] [0 0]] 0 1))))
  (testing "input graph is not modified"
    (let [snapshot classic]
      (mc/mincut classic 0 5)
      (is (= snapshot classic))))
  (testing "invalid vertices throw"
    (is (thrown? IllegalArgumentException (mc/mincut classic 0 0)))
    (is (thrown? IllegalArgumentException (mc/mincut classic 0 6)))))
