(ns clojure-algorithms.bit-manipulation.parity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.parity :refer [parity]]))

(deftest parity-test
  (testing "doctest examples"
    (is (= 0 (parity 0)))
    (is (= 1 (parity 1)))
    (is (= 1 (parity 2)))
    (is (= 0 (parity 3)))
    (is (= 0 (parity 1023))))
  (testing "negative input raises"
    (is (thrown? clojure.lang.ExceptionInfo (parity -1)))))
