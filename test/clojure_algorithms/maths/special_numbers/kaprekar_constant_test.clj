(ns clojure-algorithms.maths.special-numbers.kaprekar-constant-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.kaprekar-constant :as sut]))

(deftest test-fn-kaprekar-routine-examples
  (is (approx 3087 (sut/kaprekar-routine 3524)))
  (is (approx 8352 (sut/kaprekar-routine 3087)))
  (is (approx 6174 (sut/kaprekar-routine 8352)))
  (is (approx 6174 (sut/kaprekar-routine 6174)))
  (is (approx 999 (sut/kaprekar-routine 1)))
  (is (approx 0 (sut/kaprekar-routine 1111))))

(deftest test-fn-kaprekar-constant-examples
  (is (approx [3 [3524 3087 8352 6174]] (sut/kaprekar-constant 3524)))
  (is (approx [0 [6174]] (sut/kaprekar-constant 6174)))
  (is (approx [3 [1234 3087 8352 6174]] (sut/kaprekar-constant 1234)))
  (is (approx [-1 [1111 0]] (sut/kaprekar-constant 1111)))
  (is (approx [4 [495 9081 9621 8352 6174]] (sut/kaprekar-constant 495)))
  (is (approx [5 [9998 999 8991 8082 8532 6174]] (sut/kaprekar-constant 9998))))

(deftest test-fn-is-kaprekar-valid-examples
  (is (= true (boolean (sut/is-kaprekar-valid 3524))))
  (is (= false (boolean (sut/is-kaprekar-valid 1111))))
  (is (= true (boolean (sut/is-kaprekar-valid 1000))))
  (is (= true (boolean (sut/is-kaprekar-valid 1))))
  (is (= true (boolean (sut/is-kaprekar-valid 6174)))))

(deftest test-error-paths
  (is (thrown? Exception (sut/kaprekar-routine -1)))
  (is (thrown? Exception (sut/kaprekar-routine 10000)))
  (is (thrown? Exception (sut/kaprekar-routine 1.5)))
  (is (thrown? Exception (sut/kaprekar-constant 3524 0)))
  (is (thrown? Exception (sut/is-kaprekar-valid -1)))
  (is (= :ValueError
         (try (sut/kaprekar-routine -1)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
