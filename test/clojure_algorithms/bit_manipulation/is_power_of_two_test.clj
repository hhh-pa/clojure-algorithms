(ns clojure-algorithms.bit-manipulation.is-power-of-two-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.is-power-of-two :refer [is-power-of-two]]))

(deftest is-power-of-two-test
  (testing "doctest examples"
    (is (true? (is-power-of-two 0)))
    (is (true? (is-power-of-two 1)))
    (is (true? (is-power-of-two 2)))
    (is (true? (is-power-of-two 4)))
    (is (false? (is-power-of-two 6)))
    (is (true? (is-power-of-two 8)))
    (is (false? (is-power-of-two 17)))
    (is (true? (is-power-of-two 1024)))
    (is (false? (is-power-of-two 1023))))
  (testing "negative input raises"
    (is (thrown? clojure.lang.ExceptionInfo (is-power-of-two -1))))
  (testing "all powers of two up to 2^200"
    (is (every? is-power-of-two (take 200 (iterate #(* 2 %) 1N))))))
