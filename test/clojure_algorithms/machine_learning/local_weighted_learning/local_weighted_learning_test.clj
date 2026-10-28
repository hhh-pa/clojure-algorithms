(ns clojure-algorithms.machine-learning.local-weighted-learning.local-weighted-learning-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.local-weighted-learning.local-weighted-learning :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/weight-matrix))
  (is (some? #'sut/local-weight))
  (is (some? #'sut/local-weight-regression))
  (is (some? #'sut/load-data))
  (is (some? #'sut/plot-preds)))
