(ns clojure-algorithms.maths.laplace-transformation-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.laplace-transformation :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/laplace-transform)))
