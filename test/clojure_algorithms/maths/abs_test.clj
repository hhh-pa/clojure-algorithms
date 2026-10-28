(ns clojure-algorithms.maths.abs-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.abs :as sut]))

(deftest test-fn-abs-val-examples
  (is (approx 5.1 (sut/abs-val -5.1)))
  (is (approx (sut/abs-val -5) (sut/abs-val 5)))
  (is (approx 0 (sut/abs-val 0))))

(deftest test-fn-abs-min-examples
  (is (approx 0 (sut/abs-min [0 5 1 11])))
  (is (approx -2 (sut/abs-min [3 -10 -2])))
  (is (thrown? Exception (sut/abs-min []))))

(deftest test-fn-abs-max-examples
  (is (approx 11 (sut/abs-max [0 5 1 11])))
  (is (approx -10 (sut/abs-max [3 -10 -2])))
  (is (thrown? Exception (sut/abs-max []))))

(deftest test-fn-abs-max-sort-examples
  (is (approx 11 (sut/abs-max-sort [0 5 1 11])))
  (is (approx -10 (sut/abs-max-sort [3 -10 -2])))
  (is (thrown? Exception (sut/abs-max-sort []))))

(deftest test-fn-test-abs-val-examples
  (is (nil? (sut/test-abs-val))))
