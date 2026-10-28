(ns clojure-algorithms.bit-manipulation.missing-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.missing-number :refer [find-missing-number]]))

(deftest find-missing-number-test
  (testing "doctest examples"
    (is (= 2 (find-missing-number [0 1 3 4])))
    (is (= 2 (find-missing-number [4 3 1 0])))
    (is (= -2 (find-missing-number [-4 -3 -1 0])))
    (is (= -1 (find-missing-number [-2 2 1 3 0])))
    (is (= 2 (find-missing-number [1 3 4 5 6])))
    (is (= 3 (find-missing-number [6 5 4 2 1])))
    (is (= 2 (find-missing-number [6 1 5 3 4])))))
