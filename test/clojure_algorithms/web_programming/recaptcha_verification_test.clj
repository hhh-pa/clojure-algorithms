(ns clojure-algorithms.web-programming.recaptcha-verification-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.recaptcha-verification
             :refer [verify-payload verified? login-decision
                     login-using-recaptcha]]))

(deftest test-pure-logic
  (is (= {"secret" "s" "response" "c"} (verify-payload "s" "c")))
  (is (true? (verified? {"success" true})))
  (is (false? (verified? {"success" false})))
  (is (false? (verified? {}))))

(deftest test-login-decision
  (let [verify (fn [k] {"success" (= k "good")})
        auth (fn [[u p]] (and (= u "u") (= p "p")))]
    (is (= {:action :render :template "login.html"}
           (login-decision {:method "GET"} verify auth)))
    (is (= {:action :redirect :to "/your-webpage" :user "u"}
           (login-decision {:method "POST"
                            :params {"username" "u" "password" "p"
                                     "g-recaptcha-response" "good"}}
                           verify auth)))
    (is (= {:action :render :template "login.html"}
           (login-decision {:method "POST"
                            :params {"username" "u" "password" "p"
                                     "g-recaptcha-response" "bad"}}
                           verify auth)))
    (is (= {:action :render :template "login.html"}
           (login-decision {:method "POST"
                            :params {"username" "u" "password" "wrong"
                                     "g-recaptcha-response" "good"}}
                           verify auth)))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (login-using-recaptcha {:method "POST"}))))
