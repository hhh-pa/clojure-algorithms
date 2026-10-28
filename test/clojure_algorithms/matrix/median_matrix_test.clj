(ns clojure-algorithms.matrix.median-matrix-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.median-matrix :as sut]))

(deftest test-fn-median-examples
  (is (approx 5 (let [matrix [[1 3 5] [2 6 9] [3 6 9]]] (sut/median matrix))))
  (is (approx 3 (let [matrix [[1 2 3] [4 5 6]]] (sut/median matrix)))))
