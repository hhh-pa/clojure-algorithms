(ns clojure-algorithms.dynamic-programming.narcissistic-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.narcissistic-number :as nn]))

(deftest narcissistic-test
  (testing "narcissistic numbers"
    (is (= [0 1 2 3 4 5 6 7 8 9] (nn/find-narcissistic-numbers 10)))
    (is (= [0 1 2 3 4 5 6 7 8 9 153] (nn/find-narcissistic-numbers 160)))
    (is (= [0 1 2 3 4 5 6 7 8 9 153 370 371]
           (nn/find-narcissistic-numbers 400)))
    (is (= [0 1 2 3 4 5 6 7 8 9 153 370 371 407]
           (nn/find-narcissistic-numbers 1000)))
    (is (= [0 1 2 3 4 5 6 7 8 9 153 370 371 407 1634 8208 9474]
           (nn/find-narcissistic-numbers 10000)))
    (is (= [0] (nn/find-narcissistic-numbers 1)))
    (is (= [] (nn/find-narcissistic-numbers 0)))))
