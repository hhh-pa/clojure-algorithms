(ns clojure-algorithms.data-structures.arrays.prefix-sum-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.data-structures.arrays.prefix-sum
             :refer [make-prefix-sum get-sum contains-sum]]))

(deftest test-get-sum
  (is (= 6 (get-sum (make-prefix-sum [1 2 3]) 0 2)))
  (is (= 5 (get-sum (make-prefix-sum [1 2 3]) 1 2)))
  (is (= 3 (get-sum (make-prefix-sum [1 2 3]) 2 2)))
  (is (thrown? clojure.lang.ExceptionInfo
               (get-sum (make-prefix-sum []) 0 0)))
  (is (thrown? clojure.lang.ExceptionInfo
               (get-sum (make-prefix-sum [1 2 3]) -1 2)))
  (is (thrown? clojure.lang.ExceptionInfo
               (get-sum (make-prefix-sum [1 2 3]) 2 3)))
  (is (thrown? clojure.lang.ExceptionInfo
               (get-sum (make-prefix-sum [1 2 3]) 2 1))))

(deftest test-contains-sum
  (is (true? (contains-sum (make-prefix-sum [1 2 3]) 6)))
  (is (true? (contains-sum (make-prefix-sum [1 2 3]) 5)))
  (is (true? (contains-sum (make-prefix-sum [1 2 3]) 3)))
  (is (false? (contains-sum (make-prefix-sum [1 2 3]) 4)))
  (is (false? (contains-sum (make-prefix-sum [1 2 3]) 7)))
  (is (true? (contains-sum (make-prefix-sum [1 -2 3]) 2))))
