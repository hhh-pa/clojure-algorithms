(ns clojure-algorithms.maths.spearman-rank-correlation-coefficient-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.spearman-rank-correlation-coefficient :as sut]))

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

(deftest test-fn-assign-ranks-examples
  (is (approx [3.0 1.0 4.0 2.0 5.0] (sut/assign-ranks [3.2 1.5 4.0 2.7 5.1])))
  (is (approx [3.0 1.0 5.0 2.0 4.0] (sut/assign-ranks [10.5 8.1 12.4 9.3 11.0])))
  (is (approx [2.5 2.5 2.5 2.5] (sut/assign-ranks [1 1 1 1]))))

(deftest test-fn-calculate-spearman-rank-correlation-examples
  (is (approx -1.0 (let [x [1 2 3 4 5] y [5 4 3 2 1]] (sut/calculate-spearman-rank-correlation x y))))
  (is (approx 1.0 (let [x [1 2 3 4 5] y [2 4 6 8 10]] (sut/calculate-spearman-rank-correlation x y))))
  (is (approx 0.4 (let [x [1 2 3 4 5] y [5 1 2 9 5]] (sut/calculate-spearman-rank-correlation x y))))
  (is (thrown? Exception (let [x [5] y [9]] (sut/calculate-spearman-rank-correlation x y)))))
