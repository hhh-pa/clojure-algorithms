(ns clojure-algorithms.maths.special-numbers.armstrong-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.armstrong-numbers :as sut]))

(deftest test-fn-armstrong-number-examples
  (is (true? (every? sut/armstrong-number sut/PASSING)))
  (is (nil? (some sut/armstrong-number sut/FAILING)))
  (is (true? (sut/armstrong-number 1)))
  (is (true? (sut/armstrong-number 153)))
  (is (true? (sut/armstrong-number 370)))
  (is (true? (sut/armstrong-number 371)))
  (is (true? (sut/armstrong-number 407)))
  (is (true? (sut/armstrong-number 1634)))
  (is (false? (sut/armstrong-number 200)))
  (is (false? (sut/armstrong-number 0)))
  (is (false? (sut/armstrong-number -153)))
  (is (false? (sut/armstrong-number 1.2)))
  (is (false? (sut/armstrong-number "A")))
  (is (false? (sut/armstrong-number nil))))

(deftest test-fn-pluperfect-number-examples
  (is (true? (every? sut/pluperfect-number sut/PASSING)))
  (is (nil? (some sut/pluperfect-number sut/FAILING)))
  (is (true? (sut/pluperfect-number 370)))
  (is (false? (sut/pluperfect-number 200)))
  (is (false? (sut/pluperfect-number 0))))

(deftest test-fn-narcissistic-number-examples
  (is (true? (every? sut/narcissistic-number sut/PASSING)))
  (is (nil? (some sut/narcissistic-number sut/FAILING)))
  (is (true? (sut/narcissistic-number 371)))
  (is (false? (sut/narcissistic-number 200)))
  (is (false? (sut/narcissistic-number -1))))

(deftest test-vars-exist
  (is (some? #'sut/armstrong-number))
  (is (some? #'sut/pluperfect-number))
  (is (some? #'sut/narcissistic-number))
  (is (some? #'sut/main))
  (is (some? #'sut/PASSING))
  (is (some? #'sut/FAILING)))
