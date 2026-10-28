(ns clojure-algorithms.bit-manipulation.binary-twos-complement-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-twos-complement :refer [twos-complement]]))

(deftest twos-complement-test
  (testing "doctest examples"
    (is (= "0b0" (twos-complement 0)))
    (is (= "0b11" (twos-complement -1)))
    (is (= "0b1011" (twos-complement -5)))
    (is (= "0b101111" (twos-complement -17)))
    (is (= "0b100110001" (twos-complement -207))))
  (testing "positive input raises"
    (is (thrown? clojure.lang.ExceptionInfo (twos-complement 1)))))
