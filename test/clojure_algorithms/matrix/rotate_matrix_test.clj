(ns clojure-algorithms.matrix.rotate-matrix-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.rotate-matrix :as sut]))

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

(deftest test-fn-make-matrix-examples
  (is (approx [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]] (sut/make-matrix)))
  (is (approx [[1]] (sut/make-matrix 1)))
  (is (approx [[1 2] [3 4]] (sut/make-matrix -2)))
  (is (approx [[1 2 3] [4 5 6] [7 8 9]] (sut/make-matrix 3)))
  (is (approx (sut/make-matrix) (sut/make-matrix 4))))

(deftest test-fn-rotate-90-examples
  (is (approx [[4 8 12 16] [3 7 11 15] [2 6 10 14] [1 5 9 13]] (sut/rotate-90 (sut/make-matrix))))
  (is (approx (sut/rotate-90 (sut/make-matrix)) (sut/transpose (sut/reverse-column (sut/make-matrix))))))

(deftest test-fn-rotate-180-examples
  (is (approx [[16 15 14 13] [12 11 10 9] [8 7 6 5] [4 3 2 1]] (sut/rotate-180 (sut/make-matrix))))
  (is (approx (sut/rotate-180 (sut/make-matrix)) (sut/reverse-column (sut/reverse-row (sut/make-matrix))))))

(deftest test-fn-rotate-270-examples
  (is (approx [[13 9 5 1] [14 10 6 2] [15 11 7 3] [16 12 8 4]] (sut/rotate-270 (sut/make-matrix))))
  (is (approx (sut/rotate-270 (sut/make-matrix)) (sut/transpose (sut/reverse-row (sut/make-matrix))))))
