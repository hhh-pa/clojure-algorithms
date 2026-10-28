(ns clojure-algorithms.maths.jaccard-similarity-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.jaccard-similarity :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/jaccard-similarity)))
