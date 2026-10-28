(ns clojure-algorithms.maths.abs-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.abs :as sut]))

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

(deftest test-fn-abs-val-examples
  (is (approx 5.1 (sut/abs-val -5.1)))
  (is (approx (sut/abs-val -5) (sut/abs-val 5)))
  (is (approx 0 (sut/abs-val 0))))

(deftest test-fn-abs-min-examples
  (is (approx 0 (sut/abs-min [0 5 1 11])))
  (is (approx -2 (sut/abs-min [3 -10 -2])))
  (is (thrown? Exception (sut/abs-min []))))

(deftest test-fn-abs-max-examples
  (is (approx 11 (sut/abs-max [0 5 1 11])))
  (is (approx -10 (sut/abs-max [3 -10 -2])))
  (is (thrown? Exception (sut/abs-max []))))

(deftest test-fn-abs-max-sort-examples
  (is (approx 11 (sut/abs-max-sort [0 5 1 11])))
  (is (approx -10 (sut/abs-max-sort [3 -10 -2])))
  (is (thrown? Exception (sut/abs-max-sort []))))

(deftest test-fn-test-abs-val-examples
  (is (nil? (sut/test-abs-val))))
