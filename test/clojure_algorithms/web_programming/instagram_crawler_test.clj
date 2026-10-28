(ns clojure-algorithms.web-programming.instagram-crawler-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.instagram-crawler :as ig]))

(def ^:private sample
  {"username" "github" "full_name" "GitHub"
   "biography" "Built for developers." "business_email" "support@github.com"
   "external_url" "https://github.com/readme"
   "edge_followed_by" {"count" 200000} "edge_follow" {"count" 20}
   "edge_owner_to_timeline_media" {"count" 200}
   "profile_pic_url_hd" "https://instagram.pic/x.jpg"
   "is_verified" true "is_private" false})

(deftest test-pure-logic
  (is (= "https://www.instagram.com/github/" (ig/profile-url "github")))
  (is (= "{\"config\":1}" (ig/config-json-slice "xx{\"config\":1};")))
  (is (nil? (ig/config-json-slice "no json here")))
  (is (= "github" (ig/username_ sample)))
  (is (= "GitHub" (ig/fullname sample)))
  (is (= "Built for developers." (ig/biography sample)))
  (is (= "support@github.com" (ig/email sample)))
  (is (= "https://github.com/readme" (ig/website sample)))
  (is (= 200000 (ig/number-of-followers sample)))
  (is (= 20 (ig/number-of-followings sample)))
  (is (= 200 (ig/number-of-posts sample)))
  (is (= "https://instagram.pic/x.jpg" (ig/profile-picture-url sample)))
  (is (true? (ig/verified? sample)))
  (is (false? (ig/private? sample)))
  (is (= "GitHub (github) is Built for developers."
         (ig/describe-user sample))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (ig/fetch-user-data "github"))))
