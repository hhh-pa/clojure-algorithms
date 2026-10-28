(ns clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure-algorithms.maths.numerical-analysis.integration-by-simpson-approx :as sut]))

(deftest test-fn-simpson-integration-examples
  (is (approx 2.333 (sut/simpson-integration (fn [x] (* x x)) 1 2 3)))
  (is (thrown? AssertionError
               (sut/simpson-integration (fn [x] (* x x)) "wrong_input" 2 3)))
  (is (thrown? AssertionError
               (sut/simpson-integration (fn [x] (* x x)) 1 "wrong_input" 3)))
  (is (thrown? AssertionError
               (sut/simpson-integration (fn [x] (* x x)) 1 2 "wrong_input")))
  (is (thrown? AssertionError (sut/simpson-integration "wrong_input" 2 3 4)))
  (is (approx -2.8 (sut/simpson-integration (fn [x] (* x x)) 3.45 3.2 1)))
  (is (thrown? AssertionError
               (sut/simpson-integration (fn [x] (* x x)) 3.45 3.2 0)))
  (is (thrown? AssertionError
               (sut/simpson-integration (fn [x] (* x x)) 3.45 3.2 -1))))

(deftest test-fn-simpson-integration-defaults
  (is (approx 2.3333 (sut/simpson-integration (fn [x] (* x x)) 1 2)))
  (is (= 1000 sut/N-STEPS))
  (is (approx 4 (sut/f 2))))
