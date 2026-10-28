(ns clojure-algorithms.maths.ncr-combinations-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.ncr-combinations :as sut]))

(deftest test-fn-combinations-examples
  (is (approx 10.0 (sut/combinations 5 2)))
  (is (approx 12.375 (sut/combinations 5.5 2)))
  (is (approx 1.0 (sut/combinations 10 0)))
  (is (approx 1.0 (sut/combinations 0 0)))
  (is (thrown? Exception (sut/combinations 5 -1)))
  (is (thrown? Exception (sut/combinations 5 2.5))))
