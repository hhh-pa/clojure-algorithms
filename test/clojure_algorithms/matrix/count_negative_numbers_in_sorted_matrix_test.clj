(ns clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.count-negative-numbers-in-sorted-matrix :as sut]))

(deftest test-fn-generate-large-matrix-examples
  (is (= 1000 (count (sut/generate-large-matrix))))
  (is (= 2000 (count (first (sut/generate-large-matrix)))))
  (is (= [1000 999 998] (take 3 (first (sut/generate-large-matrix)))))
  (is (= -999 (peek (vec (first sut/grid)))))
  (is (= 1 (first (peek sut/grid))))
  (is (= -1998 (peek (peek sut/grid)))))

(deftest test-fn-validate-grid-examples
  (is (nil? (doseq [grid sut/test-grids] (sut/validate-grid grid)))))

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

(deftest test-fn-count-negatives-binary-search-examples
  (is (approx [8 0 0 3 1498500]
              (mapv sut/count-negatives-binary-search sut/test-grids))))

(deftest test-fn-count-negatives-brute-force-examples
  (is (approx [8 0 0 3 1498500]
              (mapv sut/count-negatives-brute-force sut/test-grids))))

(deftest test-fn-count-negatives-brute-force-with-break-examples
  (is (approx [8 0 0 3 1498500]
              (mapv sut/count-negatives-brute-force-with-break sut/test-grids))))

(deftest test-fn-benchmark-unsupported
  (is (thrown? Exception (sut/benchmark))))
