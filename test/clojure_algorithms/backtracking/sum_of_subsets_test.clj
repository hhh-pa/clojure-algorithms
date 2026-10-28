(ns clojure-algorithms.backtracking.sum-of-subsets-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.sum-of-subsets
             :refer [generate-sum-of-subsets-solutions create-state-space-tree]]))

(deftest generate-sum-of-subsets-solutions-test
  (testing "doctest examples"
    (is (= [[3 4 2] [4 5]]
           (generate-sum-of-subsets-solutions [3 34 4 12 5 2] 9)))
    (is (= [[3]]
           (generate-sum-of-subsets-solutions [3 34 4 12 5 2] 3)))
    (is (= []
           (generate-sum-of-subsets-solutions [3 34 4 12 5 2] 1)))))

(deftest create-state-space-tree-test
  (is (= [[1]] (create-state-space-tree [1] 1 0 [] 1)))
  (testing "every solution sums to the target"
    (doseq [sol (generate-sum-of-subsets-solutions [3 34 4 12 5 2] 9)]
      (is (= 9 (reduce + sol))))))
