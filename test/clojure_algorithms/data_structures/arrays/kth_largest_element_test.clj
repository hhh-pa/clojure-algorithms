(ns clojure-algorithms.data-structures.arrays.kth-largest-element-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.kth-largest-element
             :refer [partition-desc kth-largest-element]]))

(deftest test-partition-desc
  (is (= 4 (first (partition-desc [3 1 4 5 9 2 6 5 3 5] 0 9))))
  (is (= 1 (first (partition-desc [7 1 4 5 9 2 6 5 8] 0 8))))
  (is (= 2 (first (partition-desc ["apple" "cherry" "date" "banana"] 0 3))))
  (is (= 1 (first (partition-desc [3.1 1.2 5.6 4.7] 0 3)))))

(deftest test-kth-largest-element
  (is (= 5 (kth-largest-element [3 1 4 1 5 9 2 6 5 3 5] 3)))
  (is (= 9 (kth-largest-element [2 5 6 1 9 3 8 4 7 3 5] 1)))
  (is (= "cherry" (kth-largest-element ["apple" "cherry" "date" "banana"] 2)))
  (is (= 5.6 (kth-largest-element [3.1 1.2 5.6 4.7 7.9 5 0] 2)))
  (is (= -1 (kth-largest-element [-2 -5 -4 -1] 1)))
  (is (= -1 (kth-largest-element [] 1)))
  (is (thrown? clojure.lang.ExceptionInfo
               (kth-largest-element [2 5 6 1 9 3 8 4 7 3 5] -2)))
  (is (thrown? clojure.lang.ExceptionInfo
               (kth-largest-element [9 1 3 6 7 9 8 4 2 4 9] 110)))
  (is (thrown? clojure.lang.ExceptionInfo
               (kth-largest-element [1 2 4 3 5 9 7 6 5 9 3] 0)))
  (is (thrown? clojure.lang.ExceptionInfo
               (kth-largest-element [3.1 1.2 5.6 4.7 7.9 5 0] 1.5))))
