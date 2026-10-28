(ns clojure-algorithms.machine-learning.random-forest-regressor-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.random-forest-regressor :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/main)))
