(ns clojure-algorithms.matrix.pascal-triangle-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.pascal-triangle :as sut]))

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

(deftest test-fn-generate-pascal-triangle-examples
  (is (approx [] (sut/generate-pascal-triangle 0)))
  (is (approx [[1]] (sut/generate-pascal-triangle 1)))
  (is (approx [[1] [1 1]] (sut/generate-pascal-triangle 2)))
  (is (thrown? Exception (sut/generate-pascal-triangle -5)))
  (is (thrown? Exception (sut/generate-pascal-triangle 7.89))))

(deftest test-fn-populate-current-row-examples
  (is (approx [1 1] (let [triangle [[1]]] (sut/populate-current-row triangle 1)))))

(deftest test-fn-calculate-current-element-examples
  (is (nil? (let [triangle [[1] [1 1]] current-row [1 -1 1]] (sut/calculate-current-element triangle current-row 2 1)))))

(deftest test-fn-generate-pascal-triangle-optimized-examples
  (is (approx [[1] [1 1] [1 2 1]] (sut/generate-pascal-triangle-optimized 3)))
  (is (approx [[1]] (sut/generate-pascal-triangle-optimized 1)))
  (is (approx [] (sut/generate-pascal-triangle-optimized 0)))
  (is (thrown? Exception (sut/generate-pascal-triangle-optimized -5)))
  (is (thrown? Exception (sut/generate-pascal-triangle-optimized 7.89))))
