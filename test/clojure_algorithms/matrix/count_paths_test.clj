(ns clojure-algorithms.matrix.count-paths-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.matrix.count-paths :as sut]))

(deftest test-fn-depth-first-search-examples
  (is (= 2 (let [grid [[0 0 0 0] [1 1 0 0] [0 0 0 1] [0 1 0 0]]]
             (sut/depth-first-search grid 0 0 #{}))))
  (is (= 2 (let [grid [[0 0 0 0 0] [0 1 1 1 0] [0 1 1 1 0] [0 0 0 0 0]]]
             (sut/depth-first-search grid 0 0 #{})))))
