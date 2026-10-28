(ns clojure-algorithms.maths.special-numbers.armstrong-numbers-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.armstrong-numbers :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/armstrong-number))
  (is (some? #'sut/pluperfect-number))
  (is (some? #'sut/narcissistic-number))
  (is (some? #'sut/main))
  (is (some? #'sut/PASSING))
  (is (some? #'sut/FAILING)))
