(ns clojure-algorithms.bit-manipulation.find-previous-power-of-two-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.find-previous-power-of-two
             :refer [find-previous-power-of-two]]))

(deftest find-previous-power-of-two-test
  (testing "doctest examples"
    (is (= [0 1 2 2 4 4 4 4 8 8 8 8 8 8 8 8 16 16]
           (mapv find-previous-power-of-two (range 18)))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (find-previous-power-of-two -5)))
    (is (thrown? clojure.lang.ExceptionInfo (find-previous-power-of-two 10.5)))))
