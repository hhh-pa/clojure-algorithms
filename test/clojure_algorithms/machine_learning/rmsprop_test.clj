(ns clojure-algorithms.machine-learning.rmsprop-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.machine-learning.rmsprop :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/rmsprop)))
