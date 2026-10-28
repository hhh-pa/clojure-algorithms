(ns clojure-algorithms.bit-manipulation.binary-and-operator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-and-operator :refer [binary-and]]))

(deftest binary-and-test
  (testing "doctest examples"
    (is (= "0b000000" (binary-and 25 32)))
    (is (= "0b100000" (binary-and 37 50)))
    (is (= "0b10100" (binary-and 21 30)))
    (is (= "0b0001000" (binary-and 58 73)))
    (is (= "0b00000000" (binary-and 0 255)))
    (is (= "0b100000000" (binary-and 256 256))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (binary-and 0 -1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-and 0 1.1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-and "0" "1")))
    (is (thrown? clojure.lang.ExceptionInfo (binary-and 10 "1")))))
