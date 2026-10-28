(ns clojure-algorithms.dynamic-programming.tribonacci-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.dynamic-programming.tribonacci :as t]))

(deftest tribonacci-test
  (testing "Tribonacci prefixes"
    (is (= [0 0 1 1 2] (t/tribonacci 5)))
    (is (= [0 0 1 1 2 4 7 13] (t/tribonacci 8)))
    (is (= [0 0 1] (t/tribonacci 3)))
    (is (= [] (t/tribonacci 0)))))
