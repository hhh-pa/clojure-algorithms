(ns clojure-algorithms.maths.lucas-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.lucas-series :as sut]))

(deftest test-fn-recursive-lucas-number-examples
  (is (approx 1 (sut/recursive-lucas-number 1)))
  (is (approx 15127 (sut/recursive-lucas-number 20)))
  (is (approx 2 (sut/recursive-lucas-number 0)))
  (is (approx 167761 (sut/recursive-lucas-number 25)))
  (is (thrown? Exception (sut/recursive-lucas-number -1.5))))

(deftest test-fn-dynamic-lucas-number-examples
  (is (approx 1 (sut/dynamic-lucas-number 1)))
  (is (approx 15127 (sut/dynamic-lucas-number 20)))
  (is (approx 2 (sut/dynamic-lucas-number 0)))
  (is (approx 167761 (sut/dynamic-lucas-number 25)))
  (is (thrown? Exception (sut/dynamic-lucas-number -1.5))))
