(ns clojure-algorithms.maths.two-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.two-sum :as sut]))

(deftest test-fn-two-sum-examples
  (is (approx [0 1] (sut/two-sum [2 7 11 15] 9)))
  (is (approx [1 2] (sut/two-sum [15 2 11 7] 13)))
  (is (approx [0 3] (sut/two-sum [2 7 11 15] 17)))
  (is (approx [0 2] (sut/two-sum [7 15 11 2] 18)))
  (is (approx [2 3] (sut/two-sum [2 7 11 15] 26)))
  (is (approx [] (sut/two-sum [2 7 11 15] 8))))
