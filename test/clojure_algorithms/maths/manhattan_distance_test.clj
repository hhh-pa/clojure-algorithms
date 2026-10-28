(ns clojure-algorithms.maths.manhattan-distance-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.manhattan-distance :as sut]))

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

(deftest test-fn-manhattan-distance-examples
  (is (approx 2.0 (sut/manhattan-distance [1 1] [2 2])))
  (is (approx 1.0 (sut/manhattan-distance [1.5 1.5] [2 2])))
  (is (approx 1.5 (sut/manhattan-distance [1.5 1.5] [2.5 2])))
  (is (approx 9.0 (sut/manhattan-distance [-3 -3 -3] [0 0 0])))
  (is (thrown? Exception (sut/manhattan-distance [1 1] nil)))
  (is (thrown? Exception (sut/manhattan-distance [1 1] [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance [1 "one"] [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance 1 [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance [1 1] "not_a_list"))))

(deftest test-fn-validate-point-examples
  (is (thrown? Exception (sut/-validate-point nil)))
  (is (thrown? Exception (sut/-validate-point [1 "one"])))
  (is (thrown? Exception (sut/-validate-point 1)))
  (is (thrown? Exception (sut/-validate-point "not_a_list"))))

(deftest test-fn-manhattan-distance-one-liner-examples
  (is (approx 2.0 (sut/manhattan-distance-one-liner [1 1] [2 2])))
  (is (approx 1.0 (sut/manhattan-distance-one-liner [1.5 1.5] [2 2])))
  (is (approx 1.5 (sut/manhattan-distance-one-liner [1.5 1.5] [2.5 2])))
  (is (approx 9.0 (sut/manhattan-distance-one-liner [-3 -3 -3] [0 0 0])))
  (is (thrown? Exception (sut/manhattan-distance-one-liner [1 1] nil)))
  (is (thrown? Exception (sut/manhattan-distance-one-liner [1 1] [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance-one-liner [1 "one"] [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance-one-liner 1 [2 2 2])))
  (is (thrown? Exception (sut/manhattan-distance-one-liner [1 1] "not_a_list"))))
