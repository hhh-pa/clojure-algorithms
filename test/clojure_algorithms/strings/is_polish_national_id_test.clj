(ns clojure-algorithms.strings.is-polish-national-id-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-polish-national-id
             :refer [is-polish-national-id]]))

(deftest test-is-polish-national-id
  (testing "doctest examples"
    (is (true? (is-polish-national-id "02070803628")))
    (is (false? (is-polish-national-id "02150803629")))
    (is (false? (is-polish-national-id "02075503622")))
    (is (false? (is-polish-national-id "-99012212349")))
    (is (false? (is-polish-national-id "990122123499999")))
    (is (false? (is-polish-national-id "02070803621"))))
  (testing "invalid input"
    (is (thrown? IllegalArgumentException (is-polish-national-id 123)))
    (is (thrown? IllegalArgumentException (is-polish-national-id "abc")))))
