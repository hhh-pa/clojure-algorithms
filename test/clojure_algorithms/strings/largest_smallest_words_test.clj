(ns clojure-algorithms.strings.largest-smallest-words-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.largest-smallest-words
             :refer [find-smallest-and-largest-words]]))

(deftest test-find-smallest-and-largest-words
  (testing "doctest examples"
    (is (= ["My" "name"] (find-smallest-and-largest-words "My name is abc")))
    (is (= ["guys" "Hello"] (find-smallest-and-largest-words "Hello guys")))
    (is (= ["OnlyOneWord" "OnlyOneWord"]
           (find-smallest-and-largest-words "OnlyOneWord"))))
  (testing "edges"
    (is (= [nil nil] (find-smallest-and-largest-words "")))
    (is (= ["hi" "there"]
           (find-smallest-and-largest-words "hi, there!")))))
