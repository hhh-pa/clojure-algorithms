(ns clojure-algorithms.strings.word-occurrence-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.word-occurrence :refer [word-occurrence]]))

(deftest test-word-occurrence
  (testing "doctest examples"
    (let [s "a b A b c b d b d e f e g e h e i e j e 0"]
      (is (= (frequencies (.split ^String s " ")) (word-occurrence s))))
    (is (= {"Two" 1 "spaces" 1} (word-occurrence "Two  spaces")))))
