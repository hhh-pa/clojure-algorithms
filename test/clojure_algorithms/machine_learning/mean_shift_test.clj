(ns clojure-algorithms.machine-learning.mean-shift-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.mean-shift :as sut]))

(defn- approx
  "Tolerant equality for ported tests: exact on integers, 1e-6 on floats."
  [a b]
  (cond (and (integer? a) (integer? b)) (= a b)
        (and (number? a) (number? b))
        (let [x (double a) y (double b)]
          (or (= x y) (< (Math/abs (- x y)) 1e-6)))
        (and (sequential? a) (sequential? b))
        (and (= (count a) (count b)) (every? true? (map approx a b)))
        (and (map? a) (map? b))
        (and (= (count a) (count b))
             (every? true? (map (fn [[k v]]
                                  (and (contains? b k) (approx v (get b k))))
                                a)))
        (and (set? a) (set? b)) (= a b)
        :else (= a b)))

(deftest test-fn-euclidean-distance-examples
  (is (approx 5.0 (sut/euclidean-distance [0.0 0.0] [3.0 4.0])))
  (is (approx 0.0 (sut/euclidean-distance [1.0 1.0] [1.0 1.0])))
  (is (approx 5.0 (sut/euclidean-distance [0.0] [5.0])))
  (is (thrown? Exception (sut/euclidean-distance [0.0 0.0] [1.0]))))

(deftest test-fn-get-points-within-bandwidth-examples
  (is (approx [[0.0 0.0] [0.5 0.5]] (let [data [[0.0 0.0] [0.5 0.5] [5.0 5.0]]] (sut/get-points-within-bandwidth data [0.0 0.0] 1.0))))
  (is (approx [[5.0 5.0]] (let [data [[0.0 0.0] [0.5 0.5] [5.0 5.0]]] (sut/get-points-within-bandwidth data [5.0 5.0] 1.0))))
  (is (approx [[0.0 0.0] [0.5 0.5] [5.0 5.0]] (let [data [[0.0 0.0] [0.5 0.5] [5.0 5.0]]] (sut/get-points-within-bandwidth data [0.0 0.0] 10.0)))))

(deftest test-fn-compute-mean-examples
  (is (approx [2.0 3.0] (sut/compute-mean [[1.0 2.0] [3.0 4.0]])))
  (is (approx [0.0 0.0 0.0] (sut/compute-mean [[0.0 0.0 0.0]])))
  (is (thrown? Exception (sut/compute-mean []))))

(deftest test-fn-shift-point-examples
  (is (approx [1.25 1.25] (let [data [[1.0 1.0] [1.5 1.5] [10.0 10.0]]] (sut/shift-point [1.0 1.0] data 2.0))))
  (is (approx [10.0 10.0] (let [data [[1.0 1.0] [1.5 1.5] [10.0 10.0]]] (sut/shift-point [10.0 10.0] data 1.0)))))

(deftest test-fn-has-converged-examples
  (is (= true (boolean (sut/has-converged [1.0 1.0] [1.0000001 1.0000001] 0.0001))))
  (is (= false (boolean (sut/has-converged [1.0 1.0] [1.5 1.5] 0.0001)))))

(deftest test-fn-merge-centroids-examples
  (is (approx 2 (let [centroids [[1.0 1.0] [1.1 1.1] [10.0 10.0]] merged (sut/merge-centroids centroids 1.0)] (count merged))))
  (is (approx 3 (let [centroids [[0.0 0.0] [5.0 5.0] [10.0 10.0]] merged (sut/merge-centroids centroids 1.0)] (count (sut/merge-centroids centroids 1.0))))))
