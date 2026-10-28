(ns clojure-algorithms.web-programming.open-google-results-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.open-google-results
             :refer [encode-args-query encode-input-query search-url
                     open-first-result]]))

(deftest test-pure-logic
  (is (= "a%20b" (encode-args-query ["a" "b"])))
  (is (= "a+b" (encode-input-query "a b")))
  (is (= "https://www.google.com/search?q=a%20b&num=100"
         (search-url "a%20b"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (open-first-result "a"))))
