(ns clojure-algorithms.maths.numerical-analysis.nevilles-method-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.nevilles-method :as sut]))

(deftest test-fn-neville-interpolate-examples
  (is (approx 10.0
              (first (sut/neville-interpolate [1 2 3 4 6] [6 7 8 9 11] 5))))
  (is (approx [[0 6 0 0 0]
               [0 7 0 0 0]
               [0 8 104.0 0 0]
               [0 9 104.0 104.0 0]
               [0 11 104.0 104.0 104.0]]
              (second (sut/neville-interpolate [1 2 3 4 6] [6 7 8 9 11] 99))))
  (is (approx 104.0
              (first (sut/neville-interpolate [1 2 3 4 6] [6 7 8 9 11] 99))))
  (is (thrown? Exception (sut/neville-interpolate [1 2 3 4 6] [6 7 8 9 11] ""))))
