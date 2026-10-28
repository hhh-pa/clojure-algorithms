(ns clojure-algorithms.web-programming.current-stock-price-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.current-stock-price
             :refer [quote-url missing-tag-message stock-price]]))

(deftest test-pure-logic
  (is (= "https://finance.yahoo.com/quote/AAPL?p=AAPL" (quote-url "AAPL")))
  (is (string? missing-tag-message)))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (stock-price "GOOG"))))
