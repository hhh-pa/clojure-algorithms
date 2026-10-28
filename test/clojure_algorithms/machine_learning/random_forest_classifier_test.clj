(ns clojure-algorithms.machine-learning.random-forest-classifier-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.random-forest-classifier :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main)))
