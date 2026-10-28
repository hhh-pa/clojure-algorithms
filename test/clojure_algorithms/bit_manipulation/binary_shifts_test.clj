(ns clojure-algorithms.bit-manipulation.binary-shifts-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-shifts
             :refer [logical-left-shift logical-right-shift arithmetic-right-shift]]))

(deftest logical-left-shift-test
  (is (= "0b00" (logical-left-shift 0 1)))
  (is (= "0b10" (logical-left-shift 1 1)))
  (is (= "0b100000" (logical-left-shift 1 5)))
  (is (= "0b1000100" (logical-left-shift 17 2)))
  (is (= "0b111101111110000" (logical-left-shift 1983 4)))
  (is (thrown? clojure.lang.ExceptionInfo (logical-left-shift 1 -1))))

(deftest logical-right-shift-test
  (is (= "0b0" (logical-right-shift 0 1)))
  (is (= "0b0" (logical-right-shift 1 1)))
  (is (= "0b0" (logical-right-shift 1 5)))
  (is (= "0b100" (logical-right-shift 17 2)))
  (is (= "0b1111011" (logical-right-shift 1983 4)))
  (is (thrown? clojure.lang.ExceptionInfo (logical-right-shift 1 -1))))

(deftest arithmetic-right-shift-test
  (testing "doctest examples"
    (is (= "0b00" (arithmetic-right-shift 0 1)))
    (is (= "0b00" (arithmetic-right-shift 1 1)))
    (is (= "0b11" (arithmetic-right-shift -1 1)))
    (is (= "0b000100" (arithmetic-right-shift 17 2)))
    (is (= "0b111011" (arithmetic-right-shift -17 2)))
    (is (= "0b111110000100" (arithmetic-right-shift -1983 4)))))
