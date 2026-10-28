(ns clojure-algorithms.strings.upper-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.upper :refer [upper]]))

(deftest test-upper
  (testing "doctest examples"
    (is (= "WOW" (upper "wow")))
    (is (= "HELLO" (upper "Hello")))
    (is (= "WHAT" (upper "WHAT")))
    (is (= "WH[]32" (upper "wh[]32")))))
