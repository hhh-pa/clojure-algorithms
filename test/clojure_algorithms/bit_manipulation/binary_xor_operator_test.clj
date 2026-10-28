(ns clojure-algorithms.bit-manipulation.binary-xor-operator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-xor-operator :refer [binary-xor]]))

(deftest binary-xor-test
  (testing "doctest examples"
    (is (= "0b111001" (binary-xor 25 32)))
    (is (= "0b010111" (binary-xor 37 50)))
    (is (= "0b01011" (binary-xor 21 30)))
    (is (= "0b1110011" (binary-xor 58 73)))
    (is (= "0b11111111" (binary-xor 0 255)))
    (is (= "0b000000000" (binary-xor 256 256))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (binary-xor 0 -1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-xor 0 1.1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-xor "0" "1")))))
