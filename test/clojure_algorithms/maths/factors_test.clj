(ns clojure-algorithms.maths.factors-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.factors :as sut]))

(deftest test-fn-factors-of-a-number-examples
  (is (approx [1] (sut/factors-of-a-number 1)))
  (is (approx [1 5] (sut/factors-of-a-number 5)))
  (is (approx [1 2 3 4 6 8 12 24] (sut/factors-of-a-number 24)))
  (is (approx [] (sut/factors-of-a-number -24))))
