(ns clojure-algorithms.bit-manipulation.binary-coded-decimal-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-coded-decimal :refer [binary-coded-decimal]]))

(deftest binary-coded-decimal-test
  (testing "doctest examples"
    (is (= "0b0000" (binary-coded-decimal -2)))
    (is (= "0b0000" (binary-coded-decimal -1)))
    (is (= "0b0000" (binary-coded-decimal 0)))
    (is (= "0b0011" (binary-coded-decimal 3)))
    (is (= "0b0010" (binary-coded-decimal 2)))
    (is (= "0b00010010" (binary-coded-decimal 12)))
    (is (= "0b100110000111" (binary-coded-decimal 987)))))
