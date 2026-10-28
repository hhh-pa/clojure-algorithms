(ns clojure-algorithms.machine-learning.q-learning-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.q-learning :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/get-q-value))
  (is (some? #'sut/get-best-action))
  (is (some? #'sut/choose-action))
  (is (some? #'sut/update))
  (is (some? #'sut/get-policy))
  (is (some? #'sut/reset-env))
  (is (some? #'sut/get-available-actions-env))
  (is (some? #'sut/step-env))
  (is (some? #'sut/run-q-learning))
  (is (some? #'sut/State))
  (is (some? #'sut/LEARNING-RATE))
  (is (some? #'sut/DISCOUNT-FACTOR))
  (is (some? #'sut/EPSILON))
  (is (some? #'sut/EPSILON-DECAY))
  (is (some? #'sut/EPSILON-MIN))
  (is (some? #'sut/q-table))
  (is (some? #'sut/SIZE))
  (is (some? #'sut/GOAL))
  (is (some? #'sut/current-state)))
