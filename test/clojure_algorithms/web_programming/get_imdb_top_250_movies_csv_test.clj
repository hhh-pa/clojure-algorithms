(ns clojure-algorithms.web-programming.get-imdb-top-250-movies-csv-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure-algorithms.web-programming.get-imdb-top-250-movies-csv
             :refer [write-movies-csv get-imdb-top-250-movies default-url]])
  (:import [java.io File]))

(deftest test-write-movies-csv
  (let [tmp (File/createTempFile "imdb_" ".csv")]
    (try
      (write-movies-csv {"The Shawshank Redemption" 9.3
                         "The \"Godfather\"" 9.2}
                        (.getPath tmp))
      (let [content (slurp tmp)]
        (is (clojure.string/starts-with? content "Movie title,IMDb rating\n"))
        (is (clojure.string/includes? content "\"The Shawshank Redemption\",9.3"))
        (is (clojure.string/includes? content "\"The \"\"Godfather\"\"\",9.2")))
      (finally
        (io/delete-file tmp true)))))

(deftest test-stub-throws
  (is (thrown? clojure.lang.ExceptionInfo (get-imdb-top-250-movies)))
  (is (= "https://www.imdb.com/chart/top/?ref_=nv_mv_250" default-url)))
