(ns clojure-algorithms.web-programming.instagram-video-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.instagram-video
             :refer [api-url timestamped-filename download-video]])
  (:import [java.time LocalDateTime]))

(deftest test-pure-logic
  (is (= (str "https://downloadgram.net/wp-json/wppress/video-downloader"
              "/video?url=U")
         (api-url "U")))
  (is (= "2024-01-12_10-30-00.mp4"
         (timestamped-filename (LocalDateTime/of 2024 1 12 10 30 0)))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (download-video "U"))))
