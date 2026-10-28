(ns clojure-algorithms.maths.euler-modified-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.euler-modified :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/euler-modified)))
