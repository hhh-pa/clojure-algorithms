(ns clojure-algorithms.strings.credit-card-validator-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.strings.credit-card-validator
             :refer [validate-initial-digits luhn-validation
                     validate-credit-card-number]]))

(deftest test-validate-initial-digits
  (is (every? validate-initial-digits
              ["4111111111111111" "41111111111111" "34" "35" "37"
               "412345" "523456" "634567"]))
  (is (every? #(false? (validate-initial-digits %))
              ["14" "25" "76" "32323" "36111111111111"])))

(deftest test-luhn-validation
  (is (true? (luhn-validation "4111111111111111")))
  (is (true? (luhn-validation "36111111111111")))
  (is (false? (luhn-validation "41111111111111"))))

(deftest test-validate-credit-card-number
  (is (true? (validate-credit-card-number "4111111111111111")))
  (is (false? (validate-credit-card-number "helloworld$")))
  (is (false? (validate-credit-card-number "32323")))
  (is (false? (validate-credit-card-number "32323323233232332323")))
  (is (false? (validate-credit-card-number "36111111111111")))
  (is (false? (validate-credit-card-number "41111111111111"))))
