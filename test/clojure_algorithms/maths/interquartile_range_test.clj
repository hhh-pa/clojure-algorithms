(ns clojure-algorithms.maths.interquartile-range-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.interquartile-range :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/find-median))
  (is (some? #'sut/interquartile-range)))
