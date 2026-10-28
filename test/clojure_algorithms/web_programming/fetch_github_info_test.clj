(ns clojure-algorithms.web-programming.fetch-github-info-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-github-info
             :refer [authenticated-user-endpoint auth-headers user-token
                     fetch-github-info]]))

(deftest test-pure-logic
  (is (= "https://api.github.com/user" authenticated-user-endpoint))
  (is (= {"Authorization" "token abc"
          "Accept" "application/vnd.github.v3+json"}
         (auth-headers "abc")))
  (is (string? (user-token))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (fetch-github-info "tok"))))
