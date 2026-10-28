(ns clojure-algorithms.web-programming.instagram-pic-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.instagram-pic
             :refer [timestamped-filename saved-message download-image
                     no-meta-message]])
  (:import [java.time LocalDateTime]))

(deftest test-pure-logic
  (is (= "2024-01-12_10-30-00.jpg"
         (timestamped-filename (LocalDateTime/of 2024 1 12 10 30 0))))
  (is (re-matches #"\d{4}-\d{2}-\d{2}_\d{2}-\d{2}-\d{2}\.jpg"
                  (timestamped-filename)))
  (is (= "Image downloaded and saved in the file f.jpg"
         (saved-message "f.jpg")))
  (is (string? no-meta-message)))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (download-image "https://www.instagram.com"))))
