(ns clojure-algorithms.maths.is-int-palindrome-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-int-palindrome :as sut]))

(deftest test-fn-is-int-palindrome-examples
  (is (= false (boolean (sut/is-int-palindrome -121))))
  (is (= true (boolean (sut/is-int-palindrome 0))))
  (is (= false (boolean (sut/is-int-palindrome 10))))
  (is (= true (boolean (sut/is-int-palindrome 11))))
  (is (= true (boolean (sut/is-int-palindrome 101))))
  (is (= false (boolean (sut/is-int-palindrome 120)))))
