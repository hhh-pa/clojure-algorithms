(ns clojure-algorithms.matrix.searching-in-sorted-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.searching-in-sorted-matrix :as sut]))

(deftest test-fn-search-in-a-sorted-matrix-examples
  (is (= "Key 5 found at row- 1 column- 2\n" (with-out-str (sut/search-in-a-sorted-matrix [[2 5 7] [4 8 13] [9 11 15] [12 17 20]] 3 3 5))))
  (is (= "Key 21 not found\n" (with-out-str (sut/search-in-a-sorted-matrix [[2 5 7] [4 8 13] [9 11 15] [12 17 20]] 3 3 21))))
  (is (= "Key 2.1 found at row- 1 column- 1\n" (with-out-str (sut/search-in-a-sorted-matrix [[2.1 5 7] [4 8 13] [9 11 15] [12 17 20]] 3 3 2.1))))
  (is (= "Key 2.2 not found\n" (with-out-str (sut/search-in-a-sorted-matrix [[2.1 5 7] [4 8 13] [9 11 15] [12 17 20]] 3 3 2.2)))))
