(ns clojure-algorithms.matrix.transitive-closure-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.transitive-closure :as sut]))

(deftest test-fn-transitive-closure-examples
  (is (approx [[1 1 1 1] [1 1 1 1] [1 1 1 1] [0 0 0 1]] (let [graph [[0 1 1 0] [0 0 1 0] [1 0 0 1] [0 0 0 0]]] (sut/transitive-closure graph)))))
