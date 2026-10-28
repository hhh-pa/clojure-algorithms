(ns clojure-algorithms.web-programming.co2-emission-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.co2-emission :refer [base-url
                                                                     intensity-url
                                                                     fetch-last-half-hour
                                                                     fetch-from-to]]))

(deftest test-pure-logic
  (is (= "https://api.carbonintensity.org.uk/intensity" base-url))
  (is (= (str base-url "/2020-10-01/2020-10-03")
         (intensity-url "2020-10-01" "2020-10-03"))))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (fetch-last-half-hour)))
  (is (thrown? clojure.lang.ExceptionInfo (fetch-from-to "a" "b"))))
