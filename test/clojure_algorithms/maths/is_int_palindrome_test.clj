(ns clojure-algorithms.maths.is-int-palindrome-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.is-int-palindrome :as sut]))

(deftest test-fn-is-int-palindrome-examples
  (is (= false (sut/is-int-palindrome -121)))
  (is (= true (sut/is-int-palindrome 0)))
  (is (= false (sut/is-int-palindrome 10)))
  (is (= true (sut/is-int-palindrome 11)))
  (is (= true (sut/is-int-palindrome 101)))
  (is (= false (sut/is-int-palindrome 120))))
