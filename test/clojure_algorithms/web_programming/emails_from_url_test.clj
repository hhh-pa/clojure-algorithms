(ns clojure-algorithms.web-programming.emails-from-url-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.emails-from-url
             :refer [get-domain-name get-sub-domain-name extract-emails
                     emails-from-url]]))

(deftest test-domain-fns
  (is (= "a.b.c.d" (get-sub-domain-name "https://a.b.c.d/e/f?g=h,i=j#k")))
  (is (= "" (get-sub-domain-name "Not a URL!")))
  (is (= "c.d" (get-domain-name "https://a.b.c.d/e/f?g=h,i=j#k")))
  (is (= "" (get-domain-name "Not a URL!")))
  (is (= "github.com" (get-domain-name "https://github.com"))))

(deftest test-extract-emails
  (is (= ["a@github.com" "b@github.com"]
         (extract-emails "mail a@github.com and b@github.com, a@github.com again"
                         "github.com")))
  (is (= [] (extract-emails "no emails here" "github.com"))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (emails-from-url))))
