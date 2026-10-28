(ns clojure-algorithms.web-programming.crawl-google-scholar-citation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.crawl-google-scholar-citation
             :refer [scholar-lookup-url citation-params get-citation]]))

(deftest test-pure-logic
  (is (= "https://scholar.google.com/scholar_lookup" scholar-lookup-url))
  (is (= {"title" "T" "journal" "J" "volume" 30 "pages" "1-2"
          "year" 2018 "hl" "en"}
         (citation-params "T" "J" 30 "1-2" 2018))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (get-citation scholar-lookup-url {}))))
