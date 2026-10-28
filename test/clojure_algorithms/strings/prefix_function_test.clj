(ns clojure-algorithms.strings.prefix-function-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.prefix-function :refer [prefix-function
                                                                longest-prefix]]))

(deftest test-prefix-function
  (is (= [0 1 0 0 0 1 2 3 4] (prefix-function "aabcdaabc")))
  (is (= [0 0 0 1 2 3 4 0] (prefix-function "asdasdad"))))

(deftest test-longest-prefix
  (is (= 4 (longest-prefix "aabcdaabc")))
  (is (= 4 (longest-prefix "asdasdad")))
  (is (= 2 (longest-prefix "abcab"))))
