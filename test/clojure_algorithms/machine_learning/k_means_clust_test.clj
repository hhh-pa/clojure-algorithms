(ns clojure-algorithms.machine-learning.k-means-clust-test
  (:require [clojure-algorithms.machine-learning.k-means-clust :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private data [[0.0 0.0] [0.0 1.0] [10.0 10.0] [10.0 11.0]])

(deftest test-fn-get-initial-centroids
  (testing "k rows sampled from the data, reproducible under a seed"
    (let [c1 (sut/get-initial-centroids data 2 0)
          c2 (sut/get-initial-centroids data 2 0)]
      (is (= 2 (count c1)))
      (is (= c1 c2))
      (is (every? #(contains? (set data) %) c1)))))

(deftest test-fn-centroid-pairwise-dist
  (is (approx [[0.0 (Math/sqrt 200.0)] [1.0 (Math/sqrt 181.0)]]
              (sut/centroid-pairwise-dist [[0.0 0.0] [0.0 1.0]]
                                          [[0.0 0.0] [10.0 10.0]]))))

(deftest test-fn-assign-clusters-examples
  (is (= [0 0 1 1]
         (sut/assign-clusters data [[0.0 0.0] [10.0 10.0]]))))

(deftest test-fn-revise-centroids-examples
  (is (approx [[0.0 0.5] [10.0 10.5]]
              (sut/revise-centroids data 2 [0 0 1 1]))))

(deftest test-fn-compute-heterogeneity-examples
  (is (approx 1.0
              (sut/compute-heterogeneity data 2 [[0.0 0.5] [10.0 10.5]]
                                         [0 0 1 1]))))

(deftest test-fn-kmeans-examples
  (let [[centroids assignment] (sut/kmeans data 2 [[0.0 0.0] [10.0 10.0]] 10)]
    (is (approx [[0.0 0.5] [10.0 10.5]] centroids))
    (is (= [0 0 1 1] assignment)))
  (testing "heterogeneity history is recorded onto the atom"
    (let [history (atom [])]
      (sut/kmeans data 2 [[0.0 0.0] [10.0 10.0]] 10 history)
      (is (= [1.0] @history))))
  (testing "verbose mode logs progress"
    (let [out (with-out-str
                (sut/kmeans data 2 [[0.0 0.0] [10.0 10.0]] 10 nil true))]
      (is (str/includes? out "0")))))

(deftest test-fn-stubs
  (doseq [[f args] [[sut/plot-heterogeneity [[1.0] 1]]
                    [sut/plot-kmeans [data data [0]]]
                    [sut/report-generator [nil nil nil]]]]
    (is (= :UnsupportedOperation
           (:type (try (apply f args)
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
