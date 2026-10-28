(ns clojure-algorithms.web-programming.download-images-from-google-query-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.web-programming.download-images-from-google-query
             :refer [capped-max-images search-params query-folder
                     image-filename download-images-from-google-query]]))

(deftest test-pure-logic
  (is (= 5 (capped-max-images 5)))
  (is (= 50 (capped-max-images 500)))
  (is (= {"q" "dhaka" "tbm" "isch" "hl" "en" "ijn" "0"}
         (search-params "dhaka")))
  (is (= "query_new_york" (query-folder "new york")))
  (is (= "query_dhaka/original_size_img_0.jpg"
         (image-filename "query_dhaka" 0))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo
               (download-images-from-google-query "potato"))))
