(ns clojure-algorithms.maths.tonelli-shanks-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.tonelli-shanks :as sut]))

(deftest test-fn-legendre-symbol-examples
  (is (approx 1 (sut/legendre-symbol 2 7)))
  (is (approx -1 (sut/legendre-symbol 3 7)))
  (is (approx 0 (sut/legendre-symbol 14 7)))
  (is (approx 1 (sut/legendre-symbol 5 11))))

(deftest test-fn-tonelli-shanks-examples
  (is (approx 13 (sut/tonelli-shanks 5 41)))
  (is (approx 3 (sut/tonelli-shanks 2 7)))
  (is (approx 6 (sut/tonelli-shanks 10 13)))
  (is (approx 0 (sut/tonelli-shanks 0 11)))
  (is (approx 5 (sut/tonelli-shanks 8 17)))
  (is (thrown? Exception (sut/tonelli-shanks 3 7)))
  (is (thrown? Exception (sut/tonelli-shanks 5 4)))
  (is (thrown? Exception (sut/tonelli-shanks 5 1))))
