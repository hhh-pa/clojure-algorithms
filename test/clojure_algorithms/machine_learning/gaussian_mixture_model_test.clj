(ns clojure-algorithms.machine-learning.gaussian-mixture-model-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.gaussian-mixture-model :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/make-GaussianMixture))
  (is (some? #'sut/GaussianMixture-initialize-parameters))
  (is (some? #'sut/GaussianMixture-e-step))
  (is (some? #'sut/GaussianMixture-m-step))
  (is (some? #'sut/GaussianMixture-compute-log-likelihood))
  (is (some? #'sut/GaussianMixture-fit))
  (is (some? #'sut/GaussianMixture-predict))
  (is (some? #'sut/GaussianMixture-plot-results))
  (is (some? #'sut/TAG)))
