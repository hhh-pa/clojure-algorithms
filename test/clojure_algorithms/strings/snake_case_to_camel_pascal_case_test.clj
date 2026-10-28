(ns clojure-algorithms.strings.snake-case-to-camel-pascal-case-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.snake-case-to-camel-pascal-case
             :refer [snake-to-camel-case]]))

(deftest test-snake-to-camel-case
  (testing "doctest examples"
    (is (= "someRandomString" (snake-to-camel-case "some_random_string")))
    (is (= "SomeRandomString"
           (snake-to-camel-case "some_random_string" true)))
    (is (= "someRandomStringWithNumbers123"
           (snake-to-camel-case "some_random_string_with_numbers_123")))
    (is (= "SomeRandomStringWithNumbers123"
           (snake-to-camel-case "some_random_string_with_numbers_123" true))))
  (testing "invalid types throw"
    (is (thrown? IllegalArgumentException (snake-to-camel-case 123)))
    (is (thrown? IllegalArgumentException
                 (snake-to-camel-case "some_string" "True")))))
