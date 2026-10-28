(ns clojure-algorithms.web-programming.fetch-quotes-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-quotes :refer [today-url
                                                                     random-url
                                                                     quote-of-the-day
                                                                     random-quotes]]))

(deftest test-pure-logic
  (is (= "https://zenquotes.io/api/today" today-url))
  (is (= "https://zenquotes.io/api/random" random-url)))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (quote-of-the-day)))
  (is (thrown? clojure.lang.ExceptionInfo (random-quotes))))
