(ns clojure-algorithms.web-programming.get-top-hn-posts-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.get-top-hn-posts
             :refer [story-url stories-as-markdown get-hackernews-story
                     hackernews-top-stories]]))

(deftest test-pure-logic
  (is (= (str "https://hacker-news.firebaseio.com/v0/item/123.json"
              "?print=pretty")
         (story-url 123)))
  (is (= "* [A](http://a)\n* [B](http://b)"
         (stories-as-markdown [{:title "A" :url "http://a"}
                               {"title" "B" "url" "http://b"}]))))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (get-hackernews-story 1)))
  (is (thrown? clojure.lang.ExceptionInfo (hackernews-top-stories 2))))
