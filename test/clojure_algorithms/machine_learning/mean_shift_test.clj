(ns clojure-algorithms.machine-learning.mean-shift-test
  (:require [clojure-algorithms.machine-learning.mean-shift :as sut]
            [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]))

(deftest test-fn-euclidean-distance-examples
  (is (approx 5.0 (sut/euclidean-distance [0.0 0.0] [3.0 4.0])))
  (is (approx 0.0 (sut/euclidean-distance [1.0 1.0] [1.0 1.0])))
  (is (approx 5.0 (sut/euclidean-distance [0.0] [5.0])))
  (is (thrown? Exception (sut/euclidean-distance [0.0 0.0] [1.0]))))

(deftest test-fn-get-points-within-bandwidth-examples
  (let [data [[0.0 0.0] [0.5 0.5] [5.0 5.0]]]
    (is (approx [[0.0 0.0] [0.5 0.5]]
                (sut/get-points-within-bandwidth data [0.0 0.0] 1.0)))
    (is (approx [[5.0 5.0]] (sut/get-points-within-bandwidth data [5.0 5.0] 1.0)))
    (is (approx [[0.0 0.0] [0.5 0.5] [5.0 5.0]]
                (sut/get-points-within-bandwidth data [0.0 0.0] 10.0)))))

(deftest test-fn-compute-mean-examples
  (is (approx [2.0 3.0] (sut/compute-mean [[1.0 2.0] [3.0 4.0]])))
  (is (approx [0.0 0.0 0.0] (sut/compute-mean [[0.0 0.0 0.0]])))
  (is (thrown? Exception (sut/compute-mean []))))

(deftest test-fn-shift-point-examples
  (let [data [[1.0 1.0] [1.5 1.5] [10.0 10.0]]]
    (is (approx [1.25 1.25] (sut/shift-point [1.0 1.0] data 2.0)))
    (is (approx [10.0 10.0] (sut/shift-point [10.0 10.0] data 1.0)))))

(deftest test-fn-has-converged-examples
  (is (true? (sut/has-converged [1.0 1.0] [1.0000001 1.0000001] 1e-4)))
  (is (false? (sut/has-converged [1.0 1.0] [1.5 1.5] 1e-4))))

(deftest test-fn-merge-centroids-examples
  (is (= 2 (count (sut/merge-centroids [[1.0 1.0] [1.1 1.1] [10.0 10.0]] 1.0))))
  (is (= 3 (count (sut/merge-centroids [[0.0 0.0] [5.0 5.0] [10.0 10.0]] 1.0)))))

(deftest test-fn-mean-shift-examples
  (testing "two well-separated clusters"
    (let [data [[1.0 1.0] [1.2 1.0] [1.0 1.2] [9.0 9.0] [9.2 9.0] [9.0 9.2]]
          labels (sut/mean-shift data 2.0)]
      (is (= 2 (count (set labels))))
      (is (= (nth labels 0) (nth labels 1) (nth labels 2)))
      (is (= (nth labels 3) (nth labels 4) (nth labels 5)))
      (is (not= (nth labels 0) (nth labels 3)))))
  (testing "single cluster"
    (let [labels (sut/mean-shift [[0.0 0.0] [0.1 0.0] [0.0 0.1] [0.1 0.1]] 2.0)]
      (is (= 1 (count (set labels))))))
  (testing "one label per point"
    (is (= 4 (count (sut/mean-shift [[0.0] [5.0] [10.0] [15.0]] 1.0))))))

(deftest test-fn-mean-shift-errors
  (is (thrown? Exception (sut/mean-shift [] 1.0)))
  (is (thrown? Exception (sut/mean-shift [[1.0 2.0]] 0.0)))
  (is (thrown? Exception (sut/mean-shift [[1.0 2.0]] 1.0 0)))
  (is (thrown? Exception (sut/mean-shift [[1.0 2.0]] 1.0 300 0.0)))
  (is (= :ValueError (:type (try (sut/mean-shift [] 1.0)
                                 (catch clojure.lang.ExceptionInfo e
                                   (ex-data e)))))))
