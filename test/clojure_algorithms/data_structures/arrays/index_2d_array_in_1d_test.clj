(ns clojure-algorithms.data-structures.arrays.index-2d-array-in-1d-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.index-2d-array-in-1d
             :refer [flatten-2d index-2d-array-in-1d]]))

(deftest test-flatten-2d
  (is (= [5 -523 -1 34 0] (vec (flatten-2d [[5] [-523] [-1] [34] [0]]))))
  (is (= [5 -523 -1 34 0] (vec (flatten-2d [[5 -523 -1] [34 0]]))))
  (is (= [5 2 25 23 14 5 324 -1 0]
         (vec (flatten-2d [[5 2 25] [23 14 5] [324 -1 0]])))))

(deftest test-index-2d-array-in-1d
  (is (= 5 (index-2d-array-in-1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] 5)))
  (is (= 0 (index-2d-array-in-1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] 0)))
  (is (= 11 (index-2d-array-in-1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] 11)))
  (is (thrown? clojure.lang.ExceptionInfo
               (index-2d-array-in-1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] -1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (index-2d-array-in-1d [[0 1 2 3] [4 5 6 7] [8 9 10 11]] 12)))
  (is (thrown? clojure.lang.ExceptionInfo (index-2d-array-in-1d [[]] 0))))
