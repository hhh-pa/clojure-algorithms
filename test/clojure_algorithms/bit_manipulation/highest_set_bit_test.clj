(ns clojure-algorithms.bit-manipulation.highest-set-bit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.highest-set-bit
             :refer [get-highest-set-bit-position]]))

(deftest get-highest-set-bit-position-test
  (testing "doctest examples"
    (is (= 5 (get-highest-set-bit-position 25)))
    (is (= 6 (get-highest-set-bit-position 37)))
    (is (= 1 (get-highest-set-bit-position 1)))
    (is (= 3 (get-highest-set-bit-position 4)))
    (is (= 0 (get-highest-set-bit-position 0))))
  (testing "non-integer input raises"
    (is (thrown? clojure.lang.ExceptionInfo (get-highest-set-bit-position 0.8)))))
