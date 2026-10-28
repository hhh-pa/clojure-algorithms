(ns clojure-algorithms.matrix.matrix-equalization-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.matrix-equalization :as sut]))

(deftest test-fn-array-equalization-examples
  (is (approx 4 (sut/array-equalization [1 1 6 2 4 6 5 1 7 2 2 1 7 2 2] 4)))
  (is (approx 5 (sut/array-equalization [22 81 88 71 22 81 632 81 81 22 92] 2)))
  (is (approx 0 (sut/array-equalization [0 0 0 0 0 0 0 0 0 0 0 0] 5)))
  (is (approx 2 (sut/array-equalization [22 22 22 33 33 33] 2)))
  (is (thrown? Exception (sut/array-equalization [1 2 3] 0)))
  (is (thrown? Exception (sut/array-equalization [1 2 3] -1)))
  (is (thrown? Exception (sut/array-equalization [1 2 3] 0.5)))
  (is (approx 1 (sut/array-equalization [1 2 3] 9223372036854775807))))
