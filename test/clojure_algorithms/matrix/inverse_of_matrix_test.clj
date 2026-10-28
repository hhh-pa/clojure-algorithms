(ns clojure-algorithms.matrix.inverse-of-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.inverse-of-matrix :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/inverse-of-matrix)))
