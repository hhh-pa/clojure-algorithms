(ns clojure-algorithms.data-structures.arrays.permutations-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.permutations
             :refer [permute-recursive permute-backtrack]]))

(deftest test-permute-recursive
  (is (= [[3 2 1] [2 3 1] [1 3 2] [3 1 2] [2 1 3] [1 2 3]]
         (permute-recursive [1 2 3])))
  (is (= [[]] (permute-recursive []))))

(deftest test-permute-backtrack
  (is (= [[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 2 1] [3 1 2]]
         (permute-backtrack [1 2 3])))
  (is (= [[1]] (permute-backtrack [1]))))
