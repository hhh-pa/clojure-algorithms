(ns clojure-algorithms.neural-network.activation-functions.binary-step-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.binary-step :refer [binary-step binary-step-value]]))

(deftest binary-step-test
  (testing "doctest example"
    (is (= [0 1 1 1 0 1]
           (binary-step [-1.2 0 2 1.45 -3.7 0.3]))))
  (testing "scalar edges"
    (is (= 1 (binary-step-value 0)))
    (is (= 1 (binary-step-value 5.5)))
    (is (= 0 (binary-step-value -0.001))))
  (testing "empty input"
    (is (= [] (binary-step [])))))
