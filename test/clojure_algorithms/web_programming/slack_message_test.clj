(ns clojure-algorithms.web-programming.slack-message-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.slack-message
             :refer [message-payload slack-error-message
                     send-slack-message]]))

(deftest test-pure-logic
  (is (= {"text" "hi"} (message-payload "hi")))
  (is (= (str "Request to slack returned an error 403"
              ", the response is:\ninvalid")
         (slack-error-message 403 "invalid"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (send-slack-message "hi" "https://hooks.slack/u"))))
