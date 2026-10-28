(ns clojure-algorithms.strings.indian-phone-validator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.indian-phone-validator
             :refer [indian-phone-validator]]))

(deftest test-indian-phone-validator
  (testing "doctest examples"
    (is (false? (indian-phone-validator "+91123456789")))
    (is (true? (indian-phone-validator "+919876543210")))
    (is (false? (indian-phone-validator "01234567896")))
    (is (true? (indian-phone-validator "919876543218")))
    (is (false? (indian-phone-validator "+91-1234567899")))
    (is (true? (indian-phone-validator "+91-9876543218")))))
