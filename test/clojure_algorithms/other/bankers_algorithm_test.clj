(ns clojure-algorithms.other.bankers-algorithm-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.other.bankers-algorithm :as b]))

(deftest bankers-test
  (testing "need table matches Python doctest"
    (is (= [[1 2 0 3] [0 1 3 1] [1 1 0 2] [1 3 2 0] [2 0 0 3]]
           (b/need-table b/test-maximum-claim-table
                         b/test-allocated-resources-table))))
  (testing "available resources"
    (is (= [1 2 2 2]
           (b/available-resources b/test-claim-vector
                                  b/test-allocated-resources-table))))
  (testing "safe sequence is a permutation executing all processes"
    (let [order (b/safe-sequence b/test-claim-vector
                                 b/test-allocated-resources-table
                                 b/test-maximum-claim-table)]
      (is (= #{0 1 2 3 4} (set order)))
      (is (= 5 (count order)))))
  (testing "safe-state? true for reference data"
    (is (true? (b/safe-state? b/test-claim-vector
                              b/test-allocated-resources-table
                              b/test-maximum-claim-table))))
  (testing "unsafe state returns nil"
    (is (nil? (b/safe-sequence [3 3]
                               [[2 2] [2 2]]
                               [[3 3] [3 3]])))))
