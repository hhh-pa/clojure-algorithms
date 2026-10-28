(ns clojure-algorithms.maths.special-numbers.krishnamurthy-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.krishnamurthy-number :as sut]))

(deftest test-fn-factorial-examples
  (is (approx 6 (sut/factorial 3)))
  (is (approx 1 (sut/factorial 0)))
  (is (approx 120 (sut/factorial 5))))

(deftest test-fn-krishnamurthy-examples
  (is (= true (boolean (sut/krishnamurthy 145))))
  (is (= false (boolean (sut/krishnamurthy 240))))
  (is (= true (boolean (sut/krishnamurthy 1)))))

(deftest test-more-krishnamurthy-numbers
  (is (true? (sut/krishnamurthy 2)))
  (is (true? (sut/krishnamurthy 40585)))
  (is (false? (sut/krishnamurthy 100))))
