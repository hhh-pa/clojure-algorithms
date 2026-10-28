(ns clojure-algorithms.machine-learning.dbscan-test
  (:require [clojure-algorithms.machine-learning.dbscan :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-euclidean-distance-examples
  (is (approx 5.0 (sut/euclidean-distance [0.0 0.0] [3.0 4.0])))
  (is (approx 0.0 (sut/euclidean-distance [1.0 2.0 3.0] [1.0 2.0 3.0])))
  (is (approx 5.0 (sut/euclidean-distance [0.0] [5.0])))
  (is (thrown? Exception (sut/euclidean-distance [0.0 0.0] [1.0])))
  (is (= :ValueError (:type (try (sut/euclidean-distance [0.0] [1.0 2.0])
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))

(deftest test-fn-get-neighbors-examples
  (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]]
    (is (approx [0 1] (sut/get-neighbors data 0 0.5)))
    (is (approx [2] (sut/get-neighbors data 2 0.5)))
    (is (approx [0 1 2] (sut/get-neighbors data 0 10.0)))))

(deftest test-fn-dbscan-examples
  (testing "two well-separated clusters"
    (let [data [[1.0 1.0] [1.1 1.0] [1.0 1.1] [9.0 9.0] [9.1 9.0] [9.0 9.1]]
          labels (sut/dbscan data 0.5 2)]
      (is (= 2 (count (set labels))))
      (is (= (nth labels 0) (nth labels 1) (nth labels 2)))
      (is (= (nth labels 3) (nth labels 4) (nth labels 5)))
      (is (not= (nth labels 0) (nth labels 3)))))
  (testing "isolated noise point"
    (let [data [[0.0 0.0] [0.1 0.0] [0.0 0.1] [99.0 99.0]]
          labels (sut/dbscan data 0.5 2)]
      (is (= -1 (nth labels 3)))
      (is (= (nth labels 0) (nth labels 1) (nth labels 2)))))
  (testing "all points are noise"
    (is (approx [-1 -1] (sut/dbscan [[0.0 0.0] [5.0 5.0]] 0.3 5))))
  (testing "single cluster"
    (let [labels (sut/dbscan [[0.0 0.0] [0.1 0.0] [0.0 0.1] [0.1 0.1]] 0.5 2)]
      (is (= 1 (count (set labels))))
      (is (not (contains? (set labels) -1)))))
  (testing "border point joins the cluster without expanding it"
    (let [labels (sut/dbscan [[0.0] [0.4] [0.8]] 0.5 3)]
      (is (= (nth labels 0) (nth labels 1) (nth labels 2)))
      (is (not= -1 (nth labels 0))))))

(deftest test-fn-dbscan-errors
  (is (thrown? Exception (sut/dbscan [] 0.5 2)))
  (is (thrown? Exception (sut/dbscan [[1.0 2.0]] 0.0 2)))
  (is (thrown? Exception (sut/dbscan [[1.0 2.0]] 0.5 0)))
  (is (= :ValueError (:type (try (sut/dbscan [] 0.5 2)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))
