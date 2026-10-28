(ns clojure-algorithms.maths.prime-check-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.prime-check :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/is-prime)))

(deftest test-is-prime-values
  (testing "upstream doctest values"
    (is (true? (sut/is-prime 2)))
    (is (true? (sut/is-prime 3)))
    (is (false? (sut/is-prime 4)))
    (is (true? (sut/is-prime 17)))
    (is (false? (sut/is-prime 25)))
    (is (true? (sut/is-prime 97)))
    (is (false? (sut/is-prime 100)))))
