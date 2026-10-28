(ns clojure-algorithms.maths.combinations-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.combinations :as sut]))

(deftest test-fn-combinations-examples
  (is (approx 252 (sut/combinations 10 5)))
  (is (approx 20 (sut/combinations 6 3)))
  (is (approx 15504 (sut/combinations 20 5)))
  (is (approx 2598960 (sut/combinations 52 5)))
  (is (approx 1 (sut/combinations 0 0)))
  (is (thrown? Exception (sut/combinations -4 -5))))
