(ns clojure-algorithms.matrix.largest-square-area-in-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.largest-square-area-in-matrix :as sut]))

(deftest test-fn-largest-square-area-in-matrix-top-down-approch-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-top-down-approch 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-top-down-approch 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-top-down-approch-with-dp-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-top-down-approch-with-dp 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-top-down-approch-with-dp 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-bottom-up-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-bottom-up 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-bottom-up 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-bottom-up-space-optimization-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-bottom-up-space-optimization 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-bottom-up-space-optimization 2 2 [[0 0] [0 0]]))))
