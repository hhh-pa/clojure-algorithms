(ns clojure-algorithms.matrix.largest-square-area-in-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.largest-square-area-in-matrix :as sut]))

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

(deftest test-fn-largest-square-area-in-matrix-top-down-approch-examples
  (is (approx 0 (sut/largest-square-area-in-matrix-top-down-approch 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-top-down-approch-with-dp-examples
  (is (approx 0 (sut/largest-square-area-in-matrix-top-down-approch-with-dp 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-bottom-up-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-bottom-up 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-bottom-up 2 2 [[0 0] [0 0]]))))

(deftest test-fn-largest-square-area-in-matrix-bottom-up-space-optimization-examples
  (is (approx 2 (sut/largest-square-area-in-matrix-bottom-up-space-optimization 2 2 [[1 1] [1 1]])))
  (is (approx 0 (sut/largest-square-area-in-matrix-bottom-up-space-optimization 2 2 [[0 0] [0 0]]))))
