(ns clojure-algorithms.strings.is-contains-unique-chars-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-contains-unique-chars
             :refer [is-contains-unique-chars]]))

(deftest test-is-contains-unique-chars
  (testing "doctest examples"
    (is (true? (is-contains-unique-chars "I_love.py")))
    (is (false? (is-contains-unique-chars "I don't love Python"))))
  (testing "edges"
    (is (true? (is-contains-unique-chars "")))
    (is (true? (is-contains-unique-chars "abc")))
    (is (false? (is-contains-unique-chars "abca")))))
