(ns clojure-algorithms.maths.bailey-borwein-plouffe-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.bailey-borwein-plouffe :as sut]))

(deftest test-fn-bailey-borwein-plouffe-examples
  (is (= "6" (str (sut/bailey-borwein-plouffe 5 10000))))
  (is (thrown? Exception (sut/bailey-borwein-plouffe -10)))
  (is (thrown? Exception (sut/bailey-borwein-plouffe 0)))
  (is (thrown? Exception (sut/bailey-borwein-plouffe 1.7)))
  (is (thrown? Exception (sut/bailey-borwein-plouffe 2 -10)))
  (is (thrown? Exception (sut/bailey-borwein-plouffe 2 1.6))))
