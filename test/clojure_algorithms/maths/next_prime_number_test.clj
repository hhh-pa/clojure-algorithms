(ns clojure-algorithms.maths.next-prime-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.next-prime-number :as sut]))

(deftest test-fn-is-prime-examples
  (is (= true (boolean (sut/is-prime 2))))
  (is (= false (boolean (sut/is-prime 15))))
  (is (= true (boolean (sut/is-prime 19))))
  (is (= false (boolean (sut/is-prime 1))))
  (is (= false (boolean (sut/is-prime -7)))))

(deftest test-fn-next-prime-examples
  (is (approx 3 (sut/next-prime 2)))
  (is (approx 11 (sut/next-prime 7)))
  (is (approx 17 (sut/next-prime 14)))
  (is (approx 2 (sut/next-prime 0)))
  (is (approx 2 (sut/next-prime -10))))
