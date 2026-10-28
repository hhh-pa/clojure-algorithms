(ns clojure-algorithms.web-programming.fetch-well-rx-price-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-well-rx-price
             :refer [request-url fetch-pharmacy-and-price-list]]))

(deftest test-input-guard
  (is (nil? (fetch-pharmacy-and-price-list nil nil)))
  (is (nil? (fetch-pharmacy-and-price-list nil "30303")))
  (is (nil? (fetch-pharmacy-and-price-list "eliquis" nil))))

(deftest test-pure-logic
  (is (= "https://www.wellrx.com/prescriptions/eliquis/30303/?freshSearch=true"
         (request-url "eliquis" "30303"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (fetch-pharmacy-and-price-list "eliquis" "30303"))))
