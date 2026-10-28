(ns clojure-algorithms.hashes.sha1-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-algorithms.hashes.sha1 :refer [sha1 sha1-bytes]]))

(deftest test-sha1
  (is (= "872af2d8ac3d8695387e7c804bf0e02c18df9e6e" (sha1 "Allan")))
  (is (= "da39a3ee5e6b4b0d3255bfef95601890afd80709" (sha1 "")))
  (is (= "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3" (sha1 "test")))
  (is (= "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12" (sha1 "The quick brown fox jumps over the lazy dog")))
  (is (= (sha1 "Allan") (sha1-bytes (.getBytes "Allan" "UTF-8")))))
