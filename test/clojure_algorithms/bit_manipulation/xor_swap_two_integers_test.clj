(ns clojure-algorithms.bit-manipulation.xor-swap-two-integers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.xor-swap-two-integers :refer [xor-swap]]))

(deftest xor-swap-test
  (testing "doctest examples"
    (is (= [10 5] (xor-swap 5 10)))
    (is (= [0 0] (xor-swap 0 0)))
    (is (= [1 -1] (xor-swap -1 1)))
    (is (= [456 123] (xor-swap 123 456)))
    (is (= [54321 12345] (xor-swap 12345 54321)))))
