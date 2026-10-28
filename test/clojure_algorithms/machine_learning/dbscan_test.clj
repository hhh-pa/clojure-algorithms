(ns clojure-algorithms.machine-learning.dbscan-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.dbscan :as sut]))

(deftest test-fn-euclidean-distance-examples
  (is (approx 5.0 (sut/euclidean-distance [0.0 0.0] [3.0 4.0])))
  (is (approx 0.0 (sut/euclidean-distance [1.0 2.0 3.0] [1.0 2.0 3.0])))
  (is (approx 5.0 (sut/euclidean-distance [0.0] [5.0])))
  (is (thrown? Exception (sut/euclidean-distance [0.0 0.0] [1.0]))))

(deftest test-fn-get-neighbors-examples
  (is (approx [0 1] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 0 0.5))))
  (is (approx [2] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 2 0.5))))
  (is (approx [0 1 2] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 0 10.0)))))
