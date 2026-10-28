(ns clojure-algorithms.matrix.pascal-triangle-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.pascal-triangle :as sut]))

(deftest test-fn-print-pascal-triangle-examples
  (is (= "    1\n   1 1\n  1 2 1\n 1 3 3 1\n1 4 6 4 1\n"
         (with-out-str (sut/print-pascal-triangle 5)))))

(deftest test-fn-generate-pascal-triangle-examples
  (is (approx [] (sut/generate-pascal-triangle 0)))
  (is (approx [[1]] (sut/generate-pascal-triangle 1)))
  (is (approx [[1] [1 1]] (sut/generate-pascal-triangle 2)))
  (is (approx [[1] [1 1] [1 2 1]] (sut/generate-pascal-triangle 3)))
  (is (approx [[1] [1 1] [1 2 1] [1 3 3 1]] (sut/generate-pascal-triangle 4)))
  (is (approx [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]] (sut/generate-pascal-triangle 5)))
  (is (thrown? Exception (sut/generate-pascal-triangle -5)))
  (is (thrown? Exception (sut/generate-pascal-triangle 7.89))))

(deftest test-fn-populate-current-row-examples
  (is (approx [1 1] (let [triangle [[1]]] (sut/populate-current-row triangle 1)))))

(deftest test-fn-calculate-current-element-examples
  (is (approx [1 2 1] (let [triangle [[1] [1 1]] current-row [1 -1 1]]
                        (sut/calculate-current-element triangle current-row 2 1)))))

(deftest test-fn-generate-pascal-triangle-optimized-examples
  (is (approx [[1] [1 1] [1 2 1]] (sut/generate-pascal-triangle-optimized 3)))
  (is (approx [[1]] (sut/generate-pascal-triangle-optimized 1)))
  (is (approx [] (sut/generate-pascal-triangle-optimized 0)))
  (is (thrown? Exception (sut/generate-pascal-triangle-optimized -5)))
  (is (thrown? Exception (sut/generate-pascal-triangle-optimized 7.89))))

(deftest test-fn-benchmark-unsupported
  (is (thrown? Exception (sut/benchmark))))
