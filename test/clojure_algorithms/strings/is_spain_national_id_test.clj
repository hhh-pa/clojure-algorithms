(ns clojure-algorithms.strings.is-spain-national-id-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-spain-national-id
             :refer [is-spain-national-id]]))

(deftest test-is-spain-national-id
  (testing "doctest examples"
    (is (true? (is-spain-national-id "12345678Z")))
    (is (true? (is-spain-national-id "12345678z")))
    (is (false? (is-spain-national-id "12345678x")))
    (is (false? (is-spain-national-id "12345678I")))
    (is (true? (is-spain-national-id "12345678-Z"))))
  (testing "malformed input throws"
    (doseq [bad ["12345678" "123456709" "1234567--Z" "1234Z" "1234ZzZZ" 12345678]]
      (is (thrown? IllegalArgumentException (is-spain-national-id bad))
          (str bad)))))
