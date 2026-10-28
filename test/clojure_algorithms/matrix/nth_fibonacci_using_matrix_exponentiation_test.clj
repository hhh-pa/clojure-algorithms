(ns clojure-algorithms.matrix.nth-fibonacci-using-matrix-exponentiation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure-algorithms.matrix.nth-fibonacci-using-matrix-exponentiation :as sut]))

(deftest test-fn-nth-fibonacci-matrix-examples
  (is (approx 354224848179261915075N (sut/nth-fibonacci-matrix 100)))
  (is (approx -100 (sut/nth-fibonacci-matrix -100))))

(deftest test-fn-nth-fibonacci-bruteforce-examples
  (is (approx 354224848179261915075N (sut/nth-fibonacci-bruteforce 100)))
  (is (approx -100 (sut/nth-fibonacci-bruteforce -100))))

(deftest test-fn-main-examples
  (is (str/includes? (with-out-str (sut/main)) "10th fibonacci number")))
