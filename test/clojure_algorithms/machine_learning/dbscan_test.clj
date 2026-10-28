(ns clojure-algorithms.machine-learning.dbscan-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.machine-learning.dbscan :as sut]))

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
  (is (approx 0.0 (sut/euclidean-distance [1.0 2.0 3.0] [1.0 2.0 3.0])))
  (is (approx 5.0 (sut/euclidean-distance [0.0] [5.0])))
  (is (thrown? Exception (sut/euclidean-distance [0.0 0.0] [1.0]))))

(deftest test-fn-get-neighbors-examples
  (is (approx [0 1] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 0 0.5))))
  (is (approx [2] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 2 0.5))))
  (is (approx [0 1 2] (let [data [[0.0 0.0] [0.1 0.1] [5.0 5.0]]] (sut/get-neighbors data 0 10.0)))))
