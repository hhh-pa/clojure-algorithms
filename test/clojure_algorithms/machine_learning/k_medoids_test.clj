(ns clojure-algorithms.machine-learning.k-medoids-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.k-medoids :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/-get-data))
  (is (some? #'sut/-compute-distances))
  (is (some? #'sut/-assign-clusters))
  (is (some? #'sut/-initialize-medoids))
  (is (some? #'sut/-update-medoids))
  (is (some? #'sut/apply-k-medoids))
  (is (some? #'sut/main)))
