(ns clojure-algorithms.maths.kth-lexicographic-permutation-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.kth-lexicographic-permutation :as sut]))

(deftest test-fn-kth-permutation-examples
  (is (approx [0 1 2 3 4] (sut/kth-permutation 0 5)))
  (is (approx [1 3 0 2] (sut/kth-permutation 10 4))))
