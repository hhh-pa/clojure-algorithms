(ns clojure-algorithms.data-structures.arrays.median-two-array-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.median-two-array
             :refer [find-median-sorted-arrays]]))

(deftest test-find-median-sorted-arrays
  (is (= 2.0 (find-median-sorted-arrays [1 3] [2])))
  (is (= 2.5 (find-median-sorted-arrays [1 2] [3 4])))
  (is (= 0.0 (find-median-sorted-arrays [0 0] [0 0])))
  (is (= 1.0 (find-median-sorted-arrays [] [1])))
  (is (= 0.0 (find-median-sorted-arrays [-1000] [1000])))
  (is (= -2.75 (find-median-sorted-arrays [-1.1 -2.2] [-3.3 -4.4])))
  (is (thrown? clojure.lang.ExceptionInfo (find-median-sorted-arrays [] []))))
