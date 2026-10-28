(ns clojure-algorithms.maths.segmented-sieve-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.segmented-sieve :as sut]))

(deftest test-fn-sieve-examples
  (is (thrown? Exception (sut/sieve 0)))
  (is (thrown? Exception (sut/sieve -1)))
  (is (thrown? Exception (sut/sieve 22.2))))
