(ns clojure-algorithms.web-programming.crypto-price-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.crypto-price :refer [convert-eth
                                                                     format-conversion
                                                                     get-eth-price-usd
                                                                     eth-to-usd]]))

(deftest test-pure-logic
  (is (= 5000.0 (convert-eth 2 2500.0)))
  (is (= "2 ETH = $5000.00 USD" (format-conversion 2 5000.0))))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (get-eth-price-usd)))
  (is (thrown? clojure.lang.ExceptionInfo (eth-to-usd 1))))
