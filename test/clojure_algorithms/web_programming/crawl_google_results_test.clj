(ns clojure-algorithms.web-programming.crawl-google-results-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.crawl-google-results
             :refer [search-url crawl-and-open]]))

(deftest test-pure-logic
  (is (= "https://www.google.com/search?q=clojure sort"
         (search-url ["clojure" "sort"]))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (crawl-and-open ["x"]))))
