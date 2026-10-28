(ns clojure-algorithms.backtracking.all-permutations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.all-permutations
             :refer [generate-all-permutations create-state-space-tree]]))

(deftest generate-all-permutations-test
  (is (= [[1 2] [2 1]] (generate-all-permutations [1 2])))
  (is (= [["A" "B"] ["B" "A"]] (generate-all-permutations ["A" "B"])))
  (is (= [[1]] (generate-all-permutations [1])))
  (is (= [[]] (generate-all-permutations []))))

(deftest create-state-space-tree-test
  (is (= [[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]]
         (create-state-space-tree [1 2 3] [] 0 [false false false])))
  (is (= [["A" "B" "C"] ["A" "C" "B"] ["B" "A" "C"]
          ["B" "C" "A"] ["C" "A" "B"] ["C" "B" "A"]]
         (create-state-space-tree ["A" "B" "C"] [] 0 [false false false])))
  (is (= [[1]] (create-state-space-tree [1] [] 0 [false])))
  (testing "n! permutations of distinct elements"
    (is (= 24 (count (generate-all-permutations [3 1 2 4]))))))
