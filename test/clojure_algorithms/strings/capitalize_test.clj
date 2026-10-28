(ns clojure-algorithms.strings.capitalize-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.capitalize :refer [capitalize]]))

(deftest test-capitalize
  (testing "doctest examples"
    (is (= "Hello world" (capitalize "hello world")))
    (is (= "123 hello world" (capitalize "123 hello world")))
    (is (= " hello world" (capitalize " hello world")))
    (is (= "A" (capitalize "a")))
    (is (= "" (capitalize "")))))
