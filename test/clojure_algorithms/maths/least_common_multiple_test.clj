(ns clojure-algorithms.maths.least-common-multiple-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.least-common-multiple :as sut]))

(def ^:private test-inputs
  [[10 20] [13 15] [4 31] [10 42] [43 34] [5 12]
   [12 25] [10 25] [6 9] [0 5] [5 0] [0 0]])

(def ^:private expected-results
  [20 195 124 210 1462 60 300 50 18 0 0 0])

(deftest test-fn-least-common-multiple-slow-examples
  (is (approx 10 (sut/least-common-multiple-slow 5 2)))
  (is (approx 228 (sut/least-common-multiple-slow 12 76)))
  (is (approx 0 (sut/least-common-multiple-slow 0 5)))
  (is (approx 0 (sut/least-common-multiple-slow 5 0)))
  (is (approx 0 (sut/least-common-multiple-slow 0 0)))
  (doseq [[[a b] expected] (map vector test-inputs expected-results)]
    (is (approx expected (sut/least-common-multiple-slow a b)))))

(deftest test-fn-least-common-multiple-fast-examples
  (is (approx 10 (sut/least-common-multiple-fast 5 2)))
  (is (approx 228 (sut/least-common-multiple-fast 12 76)))
  (is (approx 0 (sut/least-common-multiple-fast 0 5)))
  (is (approx 0 (sut/least-common-multiple-fast 5 0)))
  (is (approx 0 (sut/least-common-multiple-fast 0 0)))
  (doseq [[[a b] expected] (map vector test-inputs expected-results)]
    (is (approx expected (sut/least-common-multiple-fast a b)))))

(deftest test-fn-benchmark-stub
  (is (thrown? clojure.lang.ExceptionInfo (sut/benchmark))))
