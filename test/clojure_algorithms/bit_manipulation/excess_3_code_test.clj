(ns clojure-algorithms.bit-manipulation.excess-3-code-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.bit-manipulation.excess-3-code :refer [excess-3-code]]))

(deftest excess-3-code-test
  (testing "doctest examples"
    (is (= "0b0011" (excess-3-code 0)))
    (is (= "0b0110" (excess-3-code 3)))
    (is (= "0b0101" (excess-3-code 2)))
    (is (= "0b01010011" (excess-3-code 20)))
    (is (= "0b010001010011" (excess-3-code 120)))))
