(ns clojure-algorithms.strings.is-isogram-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-isogram :refer [is-isogram]]))

(deftest test-is-isogram
  (testing "doctest examples"
    (is (true? (is-isogram "Uncopyrightable")))
    (is (false? (is-isogram "allowance"))))
  (testing "non-alphabetic input throws"
    (is (thrown? IllegalArgumentException (is-isogram "copy1")))))
