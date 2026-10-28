(ns clojure-algorithms.neural-network.activation-functions.rectified-linear-unit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.neural-network.activation-functions.rectified-linear-unit :refer [relu relu-value]]))

(deftest relu-test
  (testing "doctest example"
    (is (= [0.0 0.0 5.0] (relu [-1 0 5]))))
  (testing "scalar edges"
    (is (= 0.0 (relu-value -100)))
    (is (= 3.25 (relu-value 3.25))))
  (testing "empty input"
    (is (= [] (relu [])))))
