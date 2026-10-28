(ns clojure-algorithms.bit-manipulation.binary-count-trailing-zeros-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.binary-count-trailing-zeros :refer [binary-count-trailing-zeros]]))

(deftest binary-count-trailing-zeros-test
  (testing "doctest examples"
    (is (= 0 (binary-count-trailing-zeros 25)))
    (is (= 2 (binary-count-trailing-zeros 36)))
    (is (= 4 (binary-count-trailing-zeros 16)))
    (is (= 1 (binary-count-trailing-zeros 58)))
    (is (= 32 (binary-count-trailing-zeros 4294967296))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-trailing-zeros 0)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-trailing-zeros -10)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-trailing-zeros 0.8)))
    (is (thrown? clojure.lang.ExceptionInfo (binary-count-trailing-zeros "0")))))
