(ns clojure-algorithms.maths.softmax-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.softmax :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/softmax)))
