(ns clojure-algorithms.bit-manipulation.power-of-4-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.power-of-4 :refer [power-of-4]]))

(deftest power-of-4-test
  (testing "doctest examples"
    (is (true? (power-of-4 1)))
    (is (false? (power-of-4 2)))
    (is (true? (power-of-4 4)))
    (is (false? (power-of-4 6)))
    (is (false? (power-of-4 8)))
    (is (false? (power-of-4 17)))
    (is (true? (power-of-4 64))))
  (testing "invalid inputs"
    (is (thrown? clojure.lang.ExceptionInfo (power-of-4 0)))
    (is (thrown? clojure.lang.ExceptionInfo (power-of-4 -1)))
    (is (thrown? clojure.lang.ExceptionInfo (power-of-4 1.2)))))
