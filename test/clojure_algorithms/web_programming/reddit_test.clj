(ns clojure-algorithms.web-programming.reddit-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.reddit :refer [valid-terms
                                                               subreddit-url
                                                               validate-terms
                                                               get-subreddit-data]]))

(deftest test-pure-logic
  (is (= "https://www.reddit.com/r/learnpython/new.json?limit=1"
         (subreddit-url "learnpython" "new" 1)))
  (is (contains? valid-terms "title"))
  (is (nil? (validate-terms ["title" "url" "selftext"])))
  (is (nil? (validate-terms []))))

(deftest test-invalid-terms
  (is (thrown? IllegalArgumentException (validate-terms ["title" "bogus"])))
  (is (thrown? IllegalArgumentException
               (get-subreddit-data "x" 1 "new" ["bogus"]))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (get-subreddit-data "learnpython"))))
