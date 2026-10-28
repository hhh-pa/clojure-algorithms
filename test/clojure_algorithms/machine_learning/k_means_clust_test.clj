(ns clojure-algorithms.machine-learning.k-means-clust-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.k-means-clust :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/get-initial-centroids))
  (is (some? #'sut/centroid-pairwise-dist))
  (is (some? #'sut/assign-clusters))
  (is (some? #'sut/revise-centroids))
  (is (some? #'sut/compute-heterogeneity))
  (is (some? #'sut/plot-heterogeneity))
  (is (some? #'sut/plot-kmeans))
  (is (some? #'sut/kmeans))
  (is (some? #'sut/report-generator))
  (is (some? #'sut/TAG)))
