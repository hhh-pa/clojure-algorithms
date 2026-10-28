(ns clojure-algorithms.bit-manipulation.binary-or-operator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-or-operator :refer [binary-or]]))

(deftest binary-or-test
  (testing "doctest examples"
    (is (= "0b111001" (binary-or 25 32)))
    (is (= "0b110111" (binary-or 37 50)))
    (is (= "0b11111" (binary-or 21 30)))
    (is (= "0b1111011" (binary-or 58 73)))
    (is (= "0b11111111" (binary-or 0 255)))
    (is (= "0b100000000" (binary-or 0 256))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (binary-or 0 -1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-or 0 1.1)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-or "0" "1")))))
