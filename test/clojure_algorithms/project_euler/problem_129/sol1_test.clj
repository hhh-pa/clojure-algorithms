(ns clojure-algorithms.project-euler.problem-129.sol1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.project-euler.problem-129.sol1 :refer [least-divisible-repunit
                                                                       solution]]))

(deftest least-divisible-repunit-test
  (is (= 6 (least-divisible-repunit 7)))
  (is (= 5 (least-divisible-repunit 41)))
  (is (= 34020 (least-divisible-repunit 1234567))))

(deftest solution-test
  (is (= 17 (solution 10)))
  (is (= 109 (solution 100)))
  (is (= 1017 (solution 1000))))
