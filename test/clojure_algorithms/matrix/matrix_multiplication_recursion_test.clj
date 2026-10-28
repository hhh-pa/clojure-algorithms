(ns clojure-algorithms.matrix.matrix-multiplication-recursion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.matrix.matrix-multiplication-recursion :as sut]))

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

(deftest test-fn-is-square-examples
  (is (= true (boolean (sut/is-square []))))
  (is (= true (boolean (sut/is-square sut/matrix-1-to-4))))
  (is (= false (boolean (sut/is-square sut/matrix-5-to-9-high)))))

(deftest test-fn-matrix-multiply-examples
  (is (approx [[19 22] [43 50]] (sut/matrix-multiply sut/matrix-1-to-4 sut/matrix-5-to-8))))

(deftest test-fn-matrix-multiply-recursive-examples
  (is (approx [] (sut/matrix-multiply-recursive [] [])))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-5-to-9-wide)))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-5-to-9-high)))
  (is (thrown? Exception (sut/matrix-multiply-recursive sut/matrix-1-to-4 sut/matrix-count-up))))
