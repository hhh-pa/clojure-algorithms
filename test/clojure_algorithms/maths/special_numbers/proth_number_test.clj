(ns clojure-algorithms.maths.special-numbers.proth-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.special-numbers.proth-number :as sut]))

(deftest test-fn-proth-examples
  (is (approx 25 (sut/proth 6)))
  (is (thrown? Exception (sut/proth 0)))
  (is (thrown? Exception (sut/proth -1)))
  (is (thrown? Exception (sut/proth 6.0))))

(deftest test-fn-is-proth-number-examples
  (is (= false (boolean (sut/is-proth-number 1))))
  (is (= false (boolean (sut/is-proth-number 2))))
  (is (= true (boolean (sut/is-proth-number 3))))
  (is (= false (boolean (sut/is-proth-number 4))))
  (is (= true (boolean (sut/is-proth-number 5))))
  (is (= false (boolean (sut/is-proth-number 34))))
  (is (thrown? Exception (sut/is-proth-number -1)))
  (is (thrown? Exception (sut/is-proth-number 6.0))))
