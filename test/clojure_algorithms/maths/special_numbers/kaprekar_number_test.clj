(ns clojure-algorithms.maths.special-numbers.kaprekar-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.kaprekar-number :as sut]))

(deftest test-fn-is-kaprekar-examples
  (is (= true (boolean (sut/is-kaprekar 1))))
  (is (= true (boolean (sut/is-kaprekar 9))))
  (is (= true (boolean (sut/is-kaprekar 45))))
  (is (= true (boolean (sut/is-kaprekar 55))))
  (is (= false (boolean (sut/is-kaprekar 10))))
  (is (= false (boolean (sut/is-kaprekar 0))))
  (is (= false (boolean (sut/is-kaprekar -1))))
  (is (= true (boolean (sut/is-kaprekar 2223))))
  (is (thrown? Exception (sut/is-kaprekar 1.5))))
