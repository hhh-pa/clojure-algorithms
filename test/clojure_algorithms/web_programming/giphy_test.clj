(ns clojure-algorithms.web-programming.giphy-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.giphy :refer [format-query
                                                              search-url
                                                              extract-gif-urls
                                                              get-gifs]]))

(deftest test-pure-logic
  (is (= "space+ship" (format-query "space ship")))
  (is (= "https://api.giphy.com/v1/gifs/search?q=space+ship&api_key=K"
         (search-url "space ship" "K")))
  (is (= ["u1" "u2"] (extract-gif-urls [{"url" "u1"} {"url" "u2"}]))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (get-gifs "space ship"))))
