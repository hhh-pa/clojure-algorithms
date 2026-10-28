(ns clojure-algorithms.maths.extended-euclidean-algorithm-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.extended-euclidean-algorithm :as sut]))

(deftest test-fn-extended-euclidean-algorithm-examples
  (is (approx [1 0] (sut/extended-euclidean-algorithm 1 24)))
  (is (approx [2 -1] (sut/extended-euclidean-algorithm 8 14)))
  (is (approx [-9 47] (sut/extended-euclidean-algorithm 240 46)))
  (is (approx [1 0] (sut/extended-euclidean-algorithm 1 -4)))
  (is (approx [-1 0] (sut/extended-euclidean-algorithm -2 -4)))
  (is (approx [0 -1] (sut/extended-euclidean-algorithm 0 -4)))
  (is (approx [1 0] (sut/extended-euclidean-algorithm 2 0))))
