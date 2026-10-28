(ns clojure-algorithms.maths.three-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.three-sum :as sut]))

(deftest test-fn-three-sum-examples
  (is (approx [[-1 -1 2] [-1 0 1]] (sut/three-sum [-1 0 1 2 -1 -4])))
  (is (approx [] (sut/three-sum [1 2 3 4]))))
