(ns clojure-algorithms.bit-manipulation.find-unique-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.find-unique-number :refer [find-unique-number]]))

(deftest find-unique-number-test
  (testing "doctest examples"
    (is (= 3 (find-unique-number [1 1 2 2 3])))
    (is (= 5 (find-unique-number [4 5 4 6 6])))
    (is (= 7 (find-unique-number [7])))
    (is (= 20 (find-unique-number [10 20 10]))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (find-unique-number [])))
    (is (thrown? clojure.lang.ExceptionInfo (find-unique-number [1 "a" 1])))))
