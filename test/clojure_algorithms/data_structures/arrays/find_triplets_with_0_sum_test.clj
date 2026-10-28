(ns clojure-algorithms.data-structures.arrays.find-triplets-with-0-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.find-triplets-with-0-sum
             :refer [find-triplets-with-0-sum find-triplets-with-0-sum-hashing
                     find-triplets-with-0-sum-two-pointers]]))

(deftest test-basic
  (is (= [[-1 -1 2] [-1 0 1]]
         (find-triplets-with-0-sum [-1 0 1 2 -1 -4])))
  (is (= [] (find-triplets-with-0-sum [])))
  (is (= [[0 0 0]] (find-triplets-with-0-sum [0 0 0])))
  (is (= [[-3 0 3] [-3 1 2] [-2 -1 3] [-2 0 2] [-1 0 1]]
         (find-triplets-with-0-sum [1 2 3 0 -1 -2 -3]))))

(deftest test-hashing
  (is (= [[-1 0 1] [-1 -1 2]]
         (find-triplets-with-0-sum-hashing [-1 0 1 2 -1 -4])))
  (is (= [] (find-triplets-with-0-sum-hashing [])))
  (is (= [[0 0 0]] (find-triplets-with-0-sum-hashing [0 0 0])))
  (is (= [[-1 0 1] [-3 1 2] [-2 0 2] [-2 -1 3] [-3 0 3]]
         (find-triplets-with-0-sum-hashing [1 2 3 0 -1 -2 -3]))))

(deftest test-two-pointers
  (is (= [[-1 -1 2] [-1 0 1]]
         (find-triplets-with-0-sum-two-pointers [-1 0 1 2 -1 -4])))
  (is (= [] (find-triplets-with-0-sum-two-pointers [])))
  (is (= [[0 0 0]] (find-triplets-with-0-sum-two-pointers [0 0 0 0]))))
