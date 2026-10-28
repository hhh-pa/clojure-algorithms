(ns clojure-algorithms.backtracking.all-subsequences-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.backtracking.all-subsequences
             :refer [generate-all-subsequences create-state-space-tree]]))

(deftest generate-all-subsequences-test
  (is (= [[] [2] [1] [1 2]] (generate-all-subsequences [1 2])))
  (is (= [[] ["B"] ["A"] ["A" "B"]] (generate-all-subsequences ["A" "B"]))))

(deftest create-state-space-tree-test
  (is (= [[] [1] [2] [2 1] [3] [3 1] [3 2] [3 2 1]]
         (create-state-space-tree [3 2 1] [] 0)))
  (is (= [[] ["B"] ["A"] ["A" "B"]]
         (create-state-space-tree ["A" "B"] [] 0)))
  (is (= [[]] (create-state-space-tree [] [] 0)))
  (is (= [[] [4] [3] [3 4] [2] [2 4] [2 3] [2 3 4]
          [1] [1 4] [1 3] [1 3 4] [1 2] [1 2 4] [1 2 3] [1 2 3 4]]
         (create-state-space-tree [1 2 3 4] [] 0))))
