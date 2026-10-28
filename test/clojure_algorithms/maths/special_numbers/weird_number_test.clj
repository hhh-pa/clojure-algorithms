(ns clojure-algorithms.maths.special-numbers.weird-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.weird-number :as sut]))

(deftest test-fn-factors-examples
  (is (approx [1 2 3 4 6] (sut/factors 12)))
  (is (approx [1] (sut/factors 1)))
  (is (approx [1 2 4 5 10 20 25 50] (sut/factors 100))))

(deftest test-fn-abundant-examples
  (is (= true (boolean (sut/abundant 0))))
  (is (= false (boolean (sut/abundant 1))))
  (is (= true (boolean (sut/abundant 12))))
  (is (= false (boolean (sut/abundant 13))))
  (is (= true (boolean (sut/abundant 20)))))

(deftest test-fn-semi-perfect-examples
  (is (= true (boolean (sut/semi-perfect 0))))
  (is (= true (boolean (sut/semi-perfect 1))))
  (is (= true (boolean (sut/semi-perfect 12))))
  (is (= false (boolean (sut/semi-perfect 13)))))

(deftest test-fn-weird-examples
  (is (= false (boolean (sut/weird 0))))
  (is (= true (boolean (sut/weird 70))))
  (is (= false (boolean (sut/weird 77)))))
