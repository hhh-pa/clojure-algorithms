(ns clojure-algorithms.strings.is-srilankan-phone-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.is-srilankan-phone-number
             :refer [is-sri-lankan-phone-number]]))

(deftest test-is-sri-lankan-phone-number
  (testing "doctest examples"
    (is (true? (is-sri-lankan-phone-number "+94773283048")))
    (is (true? (is-sri-lankan-phone-number "+9477-3283048")))
    (is (true? (is-sri-lankan-phone-number "0718382399")))
    (is (true? (is-sri-lankan-phone-number "0094702343221")))
    (is (true? (is-sri-lankan-phone-number "075 3201568")))
    (is (false? (is-sri-lankan-phone-number "07779209245")))
    (is (false? (is-sri-lankan-phone-number "0957651234")))))
