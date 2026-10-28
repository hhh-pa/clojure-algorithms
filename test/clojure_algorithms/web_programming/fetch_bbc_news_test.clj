(ns clojure-algorithms.web-programming.fetch-bbc-news-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-bbc-news :refer [request-url
                                                                       format-headline
                                                                       fetch-bbc-news]]))

(deftest test-pure-logic
  (is (= (str "https://newsapi.org/v1/articles?source=bbc-news"
              "&sortBy=top&apiKey=KEY")
         (request-url "KEY")))
  (is (= "1.) Big news" (format-headline 1 {"title" "Big news"}))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (fetch-bbc-news "KEY"))))
