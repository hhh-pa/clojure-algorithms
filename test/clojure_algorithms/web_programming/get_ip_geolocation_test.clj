(ns clojure-algorithms.web-programming.get-ip-geolocation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.get-ip-geolocation
             :refer [api-url format-location get-ip-geolocation]]))

(deftest test-pure-logic
  (is (= "https://ipinfo.io/8.8.8.8/json" (api-url "8.8.8.8")))
  (is (= "Location: Mountain View, California, US"
         (format-location {"city" "Mountain View" "region" "California"
                           "country" "US"})))
  (is (= "Location data not found." (format-location {"ip" "8.8.8.8"}))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (get-ip-geolocation "8.8.8.8"))))
