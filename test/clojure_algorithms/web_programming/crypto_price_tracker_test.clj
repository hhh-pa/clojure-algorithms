(ns clojure-algorithms.web-programming.crypto-price-tracker-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.crypto-price-tracker
             :refer [price-url extract-price crypto-price]]))

(deftest test-pure-logic
  (is (= (str "https://api.coingecko.com/api/v3/simple/price"
              "?ids=bitcoin&vs_currencies=usd")
         (price-url "bitcoin")))
  (is (= 42000.5 (extract-price {"bitcoin" {"usd" 42000.5}} "bitcoin")))
  (is (= 0.0 (extract-price {} "bitcoin")))
  (is (= 0.0 (extract-price {"bitcoin" {}} "bitcoin"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (crypto-price)))
  (is (thrown? clojure.lang.ExceptionInfo (crypto-price "ethereum"))))
