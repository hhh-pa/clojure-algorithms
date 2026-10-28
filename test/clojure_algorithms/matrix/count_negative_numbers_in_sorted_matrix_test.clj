(ns clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix :as sut]))

(deftest test-fn-generate-large-matrix-examples
  (is (some? (sut/generate-large-matrix))))

(deftest test-fn-find-negative-index-examples
  (is (approx 4 (sut/find-negative-index [0 0 0 0])))
  (is (approx 3 (sut/find-negative-index [4 3 2 -1])))
  (is (approx 2 (sut/find-negative-index [1 0 -1 -10])))
  (is (approx 3 (sut/find-negative-index [0 0 0 -1])))
  (is (approx 3 (sut/find-negative-index [11 8 7 -3 -5 -9])))
  (is (approx 0 (sut/find-negative-index [-1 -1 -2 -3])))
  (is (approx 3 (sut/find-negative-index [5 1 0])))
  (is (approx 0 (sut/find-negative-index [-5 -5 -5])))
  (is (approx 1 (sut/find-negative-index [0])))
  (is (approx 0 (sut/find-negative-index []))))
