(ns clojure-algorithms.bit-manipulation.rotate-bits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.rotate-bits :refer [rotate-left32 rotate-right32]]))

(deftest rotate-left32-test
  (testing "doctest examples"
    (is (= 2 (rotate-left32 1 1)))
    (is (= 2147483648 (rotate-left32 1 31)))
    (is (= 1 (rotate-left32 0x80000000 1)))
    (is (= 591751041 (rotate-left32 0x12345678 4))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (rotate-left32 -1 3)))
    (is (thrown? clojure.lang.ExceptionInfo (rotate-left32 1 -1)))))

(deftest rotate-right32-test
  (testing "doctest examples"
    (is (= 1 (rotate-right32 2 1)))
    (is (= 2147483648 (rotate-right32 1 1)))
    (is (= 2166572391 (rotate-right32 0x12345678 4))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (rotate-right32 -1 1)))
    (is (thrown? clojure.lang.ExceptionInfo (rotate-right32 1 -3))))
  (testing "left and right are inverses"
    (is (= 0x12345678 (rotate-right32 (rotate-left32 0x12345678 4) 4)))
    (is (= 0x12345678 (rotate-left32 (rotate-right32 0x12345678 4) 4)))))
