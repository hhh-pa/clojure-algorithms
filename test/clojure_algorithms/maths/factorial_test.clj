(ns clojure-algorithms.maths.factorial-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.factorial :as sut]))

(deftest test-fn-factorial-examples
  (is (approx 1 (sut/factorial 0)))
  (is (approx 1 (sut/factorial 1)))
  (is (approx 120 (sut/factorial 5)))
  (is (thrown? Exception (sut/factorial 0.1)))
  (is (thrown? Exception (sut/factorial -1))))

(deftest test-fn-factorial-recursive-examples
  (is (approx 1 (sut/factorial-recursive 0)))
  (is (approx 1 (sut/factorial-recursive 1)))
  (is (approx 120 (sut/factorial-recursive 5)))
  (is (thrown? Exception (sut/factorial-recursive 0.1)))
  (is (thrown? Exception (sut/factorial-recursive -1))))
