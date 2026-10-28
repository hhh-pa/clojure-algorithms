(ns clojure-algorithms.web-programming.fetch-anime-and-play-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.fetch-anime-and-play
             :refer [search-url episode-page-url download-url search-scraper
                     search-anime-episode-list get-anime-episode]]))

(deftest test-pure-logic
  (is (= "https://ww7.gogoanime2.org/search?keyword=demon_slayer"
         (search-url "demon_slayer")))
  (is (= "https://ww7.gogoanime2.org/anime/kimetsu-no-yaiba"
         (episode-page-url "/anime/kimetsu-no-yaiba")))
  (is (= "https://x/playlist/abc.m3u8"
         (download-url "https://x/embed/abc"))))

(deftest test-stubs-throw
  (is (thrown? clojure.lang.ExceptionInfo (search-scraper "x")))
  (is (thrown? clojure.lang.ExceptionInfo (search-anime-episode-list "/a")))
  (is (thrown? clojure.lang.ExceptionInfo (get-anime-episode "/w"))))
