(ns clojure-algorithms.strings.reverse-words-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.reverse-words :refer [reverse-words]]))

(deftest test-reverse-words
  (testing "doctest examples"
    (is (= "Python love I" (reverse-words "I love Python")))
    (is (= "Python Love I" (reverse-words "I     Love          Python")))))
