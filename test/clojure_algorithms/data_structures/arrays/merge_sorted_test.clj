(ns clojure-algorithms.data-structures.arrays.merge-sorted-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.merge-sorted
             :refer [merge-sorted-arrays]]))

(deftest test-merge-sorted-arrays
  (is (= [1 2 3 4 5 6] (merge-sorted-arrays [1 3 5] [2 4 6])))
  (is (= [1 2] (merge-sorted-arrays [1 2] [])))
  (is (= [3 4] (merge-sorted-arrays [] [3 4])))
  (is (= [] (merge-sorted-arrays [] [])))
  (is (= [0 0 0 0] (merge-sorted-arrays [0 0] [0 0])))
  (is (= [-5 -3 -2 -2 -1] (merge-sorted-arrays [-5 -3 -1] [-2 -2])))
  (is (= [0 0 1 1 2 2 3 3 4 4]
         (merge-sorted-arrays (range 5) (range 5))))
  (is (thrown? clojure.lang.ExceptionInfo (merge-sorted-arrays [1 -1] [])))
  (is (thrown? clojure.lang.ExceptionInfo (merge-sorted-arrays [] [1 -1]))))
