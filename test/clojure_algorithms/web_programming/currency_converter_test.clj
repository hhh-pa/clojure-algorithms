(ns clojure-algorithms.web-programming.currency-converter-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.currency-converter
             :refer [currencies currency-description request-params
                     convert-currency]]))

(deftest test-pure-logic
  (is (= "United States Dollar" (currency-description "USD")))
  (is (= "Indian Rupee" (currency-description "inr")))
  (is (nil? (currency-description "XXX")))
  (is (= 153 (count currencies)))
  (is (= {"from" "USD" "to" "INR" "amount" 1.0 "api_key" "k"}
         (request-params "USD" "INR" 1.0 "k"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (convert-currency))))
