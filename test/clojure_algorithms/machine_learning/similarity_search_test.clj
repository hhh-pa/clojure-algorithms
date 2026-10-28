(ns clojure-algorithms.machine-learning.similarity-search-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.similarity-search :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/euclidean))
  (is (some? #'sut/similarity-search))
  (is (some? #'sut/cosine-similarity)))
