(ns clojure-algorithms.data-structures.arrays.rotate-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.rotate-array
             :refer [rotate-array]]))

(deftest test-rotate-array
  (is (= [4 5 1 2 3] (rotate-array [1 2 3 4 5] 2)))
  (is (= [3 4 5 1 2] (rotate-array [1 2 3 4 5] -2)))
  (is (= [4 5 1 2 3] (rotate-array [1 2 3 4 5] 7)))
  (is (= [] (rotate-array [] 3)))
  (is (= [1 2 3] (rotate-array [1 2 3] 0)))
  (is (= [1 2 3] (rotate-array [1 2 3] 3))))
