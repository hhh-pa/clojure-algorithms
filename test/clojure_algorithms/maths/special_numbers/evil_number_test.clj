(ns clojure-algorithms.maths.special-numbers.evil-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.evil-number :as sut]))

(deftest test-fn-is-evil-number-examples
  (is (= true (boolean (sut/is-evil-number 0))))
  (is (= true (boolean (sut/is-evil-number 3))))
  (is (= true (boolean (sut/is-evil-number 5))))
  (is (= true (boolean (sut/is-evil-number 6))))
  (is (= true (boolean (sut/is-evil-number 9))))
  (is (= false (boolean (sut/is-evil-number 1))))
  (is (= false (boolean (sut/is-evil-number 2))))
  (is (= false (boolean (sut/is-evil-number 4))))
  (is (= false (boolean (sut/is-evil-number 7))))
  (is (= false (boolean (sut/is-evil-number 8))))
  (is (thrown? Exception (sut/is-evil-number -1)))
  (is (thrown? Exception (sut/is-evil-number 5.5))))
