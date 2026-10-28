(ns clojure-algorithms.strings.min-window-substring-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.min-window-substring :refer [min-window]]))

(deftest test-min-window
  (testing "doctest examples"
    (is (= "llo W" (min-window "Hello World" "lWl")))
    (is (= "" (min-window "Hello World" "f"))))
  (testing "classic cases"
    (is (= "BANC" (min-window "ADOBECODEBANC" "ABC")))
    (is (= "a" (min-window "a" "a")))
    (is (= "" (min-window "a" "aa")))))
