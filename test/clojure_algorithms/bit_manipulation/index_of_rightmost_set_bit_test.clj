(ns clojure-algorithms.bit-manipulation.index-of-rightmost-set-bit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.index-of-rightmost-set-bit
             :refer [get-index-of-rightmost-set-bit]]))

(deftest get-index-of-rightmost-set-bit-test
  (testing "doctest examples"
    (is (= -1 (get-index-of-rightmost-set-bit 0)))
    (is (= 0 (get-index-of-rightmost-set-bit 5)))
    (is (= 2 (get-index-of-rightmost-set-bit 36)))
    (is (= 3 (get-index-of-rightmost-set-bit 8))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (get-index-of-rightmost-set-bit -18)))
    (is (thrown? clojure.lang.ExceptionInfo (get-index-of-rightmost-set-bit "test")))
    (is (thrown? clojure.lang.ExceptionInfo (get-index-of-rightmost-set-bit 1.25)))))
