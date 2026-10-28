(ns clojure-algorithms.machine-learning.t-stochastic-neighbour-embedding-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.t-stochastic-neighbour-embedding :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/collect-dataset))
  (is (some? #'sut/compute-pairwise-affinities))
  (is (some? #'sut/compute-low-dim-affinities))
  (is (some? #'sut/apply-tsne))
  (is (some? #'sut/main)))
