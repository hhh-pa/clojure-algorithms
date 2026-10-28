(ns clojure-algorithms.matrix.count-paths-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.count-paths :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/depth-first-search)))
