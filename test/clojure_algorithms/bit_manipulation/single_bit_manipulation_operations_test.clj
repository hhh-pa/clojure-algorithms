(ns clojure-algorithms.bit-manipulation.single-bit-manipulation-operations-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.single-bit-manipulation-operations
             :refer [set-bit clear-bit flip-bit is-bit-set get-bit
                     clear-least-significant-set-bit]]))

(deftest single-bit-ops-test
  (testing "set-bit"
    (is (= 15 (set-bit 0xD 1)))
    (is (= 32 (set-bit 0 5)))
    (is (= 15 (set-bit 15 1))))
  (testing "clear-bit"
    (is (= 16 (clear-bit 0x12 1)))
    (is (= 0 (clear-bit 0 5))))
  (testing "flip-bit"
    (is (= 7 (flip-bit 5 1)))
    (is (= 4 (flip-bit 5 0))))
  (testing "is-bit-set"
    (is (false? (is-bit-set 0xA 0)))
    (is (true? (is-bit-set 0xA 1)))
    (is (false? (is-bit-set 0xA 2)))
    (is (true? (is-bit-set 0xA 3)))
    (is (false? (is-bit-set 0 17))))
  (testing "get-bit"
    (is (= 0 (get-bit 0xA 0)))
    (is (= 1 (get-bit 0xA 1)))
    (is (= 0 (get-bit 0xA 2)))
    (is (= 1 (get-bit 0xA 3))))
  (testing "clear-least-significant-set-bit"
    (is (= 40 (clear-least-significant-set-bit 0x2C)))
    (is (= 0 (clear-least-significant-set-bit 8)))
    (is (= 0 (clear-least-significant-set-bit 0)))
    (is (= 14 (clear-least-significant-set-bit 15)))
    (is (= -6 (clear-least-significant-set-bit -5)))))
