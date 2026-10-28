(ns clojure-algorithms.machine-learning.k-nearest-neighbors-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.k-nearest-neighbors :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-KNN))
  (is (some? #'sut/KNN-euclidean-distance))
  (is (some? #'sut/KNN-classify)))
