(ns clojure-algorithms.maths.basic-maths-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.basic-maths :as sut]))

(deftest test-fn-prime-factors-examples
  (is (approx [2 2 5 5] (sut/prime-factors 100)))
  (is (thrown? Exception (sut/prime-factors 0)))
  (is (thrown? Exception (sut/prime-factors -10))))

(deftest test-fn-number-of-divisors-examples
  (is (approx 9 (sut/number-of-divisors 100)))
  (is (thrown? Exception (sut/number-of-divisors 0)))
  (is (thrown? Exception (sut/number-of-divisors -10))))

(deftest test-fn-sum-of-divisors-examples
  (is (approx 217 (sut/sum-of-divisors 100)))
  (is (thrown? Exception (sut/sum-of-divisors 0)))
  (is (thrown? Exception (sut/sum-of-divisors -10))))

(deftest test-fn-euler-phi-examples
  (is (approx 40 (sut/euler-phi 100)))
  (is (thrown? Exception (sut/euler-phi 0)))
  (is (thrown? Exception (sut/euler-phi -10))))
