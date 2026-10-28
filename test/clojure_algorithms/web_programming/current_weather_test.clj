(ns clojure-algorithms.web-programming.current-weather-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.current-weather
             :refer [current-weather openweathermap-params
                     weatherstack-params]]))

(deftest test-pure-logic
  (is (= {"q" "Paris" "appid" "k"}
         (openweathermap-params "Paris" "k")))
  (is (= {"query" "Paris" "access_key" "k"}
         (weatherstack-params "Paris" "k"))))

(deftest test-no-keys-throws
  (is (thrown? IllegalArgumentException (current-weather "location"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (current-weather "Paris" "key" ""))))
