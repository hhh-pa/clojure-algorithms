(ns clojure-algorithms.maths.triplet-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.triplet-sum :as sut]))

(deftest test-fn-triplet-sum1-examples
  (is (approx [5 7 23] (sut/triplet-sum1 [13 29 7 23 5] 35)))
  (is (approx [9 19 37] (sut/triplet-sum1 [37 9 19 50 44] 65)))
  (is (approx [0 0 0] (let [arr [6 47 27 1 15] target 11] (sut/triplet-sum1 arr target)))))

(deftest test-fn-triplet-sum2-examples
  (is (approx [5 7 23] (sut/triplet-sum2 [13 29 7 23 5] 35)))
  (is (approx [9 19 37] (sut/triplet-sum2 [37 9 19 50 44] 65)))
  (is (approx [0 0 0] (let [arr [6 47 27 1 15] target 11] (sut/triplet-sum2 arr target)))))
