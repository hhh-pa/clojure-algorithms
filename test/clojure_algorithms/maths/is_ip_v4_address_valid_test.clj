(ns clojure-algorithms.maths.is-ip-v4-address-valid-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.is-ip-v4-address-valid :as sut]))

(deftest test-fn-is-ip-v4-address-valid-examples
  (is (= true (boolean (sut/is-ip-v4-address-valid "192.168.0.23"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "192.256.15.8"))))
  (is (= true (boolean (sut/is-ip-v4-address-valid "172.100.0.8"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "255.256.0.256"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.33333333.4"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.-3.4"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3.4.5"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.A.4"))))
  (is (= true (boolean (sut/is-ip-v4-address-valid "0.0.0.0"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3."))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3.05")))))
