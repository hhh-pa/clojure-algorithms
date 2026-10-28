(ns clojure-algorithms.maths.chinese-remainder-theorem-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.chinese-remainder-theorem :as sut]))

(deftest test-fn-extended-euclid-examples
  (is (approx [-1 2] (sut/extended-euclid 10 6)))
  (is (approx [-2 3] (sut/extended-euclid 7 5))))

(deftest test-fn-chinese-remainder-theorem-examples
  (is (approx 31 (sut/chinese-remainder-theorem 5 1 7 3)))
  (is (approx 14 (sut/chinese-remainder-theorem 6 1 4 3))))

(deftest test-fn-invert-modulo-examples
  (is (approx 3 (sut/invert-modulo 2 5)))
  (is (approx 1 (sut/invert-modulo 8 7))))

(deftest test-fn-chinese-remainder-theorem2-examples
  (is (approx 31 (sut/chinese-remainder-theorem2 5 1 7 3)))
  (is (approx 14 (sut/chinese-remainder-theorem2 6 1 4 3))))
