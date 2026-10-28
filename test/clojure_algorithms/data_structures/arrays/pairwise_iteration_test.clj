(ns clojure-algorithms.data-structures.arrays.pairwise-iteration-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.pairwise-iteration
             :refer [pairwise-iteration-tee pairwise-iteration-comprehension]]))

(deftest test-tee
  (is (= [[1 2] [2 3]] (vec (pairwise-iteration-tee [1 2 3]))))
  (is (= [[4 3] [3 5]] (vec (pairwise-iteration-tee [4 3 5]))))
  (is (= [[\2 \3] [\3 \4] [\4 \5]] (vec (pairwise-iteration-tee "2345"))))
  (is (= [["ATG" "GCT"] ["GCT" "TGC"] ["TGC" "TAA"]]
         (vec (pairwise-iteration-tee ["ATG" "GCT" "TGC" "TAA"]))))
  (is (= [] (vec (pairwise-iteration-tee ["a"]))))
  (is (= [] (vec (pairwise-iteration-tee [])))))

(deftest test-comprehension
  (is (= [[0 2] [2 4] [4 6]]
         (vec (pairwise-iteration-comprehension [0 1 2 3 4 5 6] 2))))
  (is (= [[0 3] [3 6]]
         (vec (pairwise-iteration-comprehension [0 1 2 3 4 5 6] 3))))
  (is (= [[0 2] [2 4]]
         (vec (pairwise-iteration-comprehension [0 1 2 3 4] 2))))
  (is (= [["ATG" "GCT"] ["GCT" "TGC"] ["TGC" "TAA"]]
         (vec (pairwise-iteration-comprehension ["ATG" "GCT" "TGC" "TAA"]))))
  (is (= [] (vec (pairwise-iteration-comprehension ["a"] 1)))))
