(ns clojure-algorithms.strings.camel-case-to-snake-case-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.camel-case-to-snake-case
             :refer [camel-to-snake-case]]))

(deftest test-camel-to-snake-case
  (testing "doctest examples"
    (is (= "some_random_string" (camel-to-snake-case "someRandomString")))
    (is (= "some_random_str_ng" (camel-to-snake-case "SomeRandomStr#ng")))
    (is (= "123_some_random_123_string_123"
           (camel-to-snake-case "123someRandom123String123")))
    (is (= "123_some_random_123_string_123"
           (camel-to-snake-case "123SomeRandom123String123"))))
  (testing "non-string input throws"
    (is (thrown? IllegalArgumentException (camel-to-snake-case 123)))))
