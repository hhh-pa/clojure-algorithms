(ns clojure-algorithms.web-programming.nasa-data-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.nasa-data :refer [apod-params
                                                                  archive-params
                                                                  image-name
                                                                  save-path
                                                                  get-apod-data
                                                                  save-apod
                                                                  get-archive-data]]))

(deftest test-pure-logic
  (is (= {"api_key" "K"} (apod-params "K")))
  (is (= {"q" "apollo 2011"} (archive-params "apollo 2011")))
  (is (= "pic.jpg" (image-name "https://x/y/pic.jpg")))
  (is (= "./pic.jpg" (save-path "." "pic.jpg"))))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (get-apod-data "K")))
  (is (thrown? clojure.lang.ExceptionInfo (save-apod "K")))
  (is (thrown? clojure.lang.ExceptionInfo (get-archive-data "apollo"))))
