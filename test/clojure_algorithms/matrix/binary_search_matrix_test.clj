(ns clojure-algorithms.matrix.binary-search-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.binary-search-matrix :as sut]))

(deftest test-fn-mat-bin-search-examples
  (is (approx [0 0] (let [matrix [[1 4 7 11 15] [2 5 8 12 19] [3 6 9 16 22] [10 13 14 17 24] [18 21 23 26 30]] target 1] (sut/mat-bin-search target matrix))))
  (is (approx [-1 -1] (let [matrix [[1 4 7 11 15] [2 5 8 12 19] [3 6 9 16 22] [10 13 14 17 24] [18 21 23 26 30]] target 34] (sut/mat-bin-search target matrix)))))
