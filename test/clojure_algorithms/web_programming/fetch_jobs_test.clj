(ns clojure-algorithms.web-programming.fetch-jobs-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-jobs :refer [jobs-url
                                                                   fetch-jobs]]))

(deftest test-pure-logic
  (is (= (str "https://www.indeed.co.in/jobs?q=mobile+app+development"
              "&l=Bangalore")
         (jobs-url "Bangalore"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (fetch-jobs "mumbai"))))
