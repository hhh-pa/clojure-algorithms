(ns clojure-algorithms.maths.perfect-cube-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.perfect-cube :as sut]))

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

(deftest test-fn-perfect-cube-examples
  (is (= true (boolean (sut/perfect-cube 27))))
  (is (= false (boolean (sut/perfect-cube 4)))))

(deftest test-fn-perfect-cube-binary-search-examples
  (is (= true (boolean (sut/perfect-cube-binary-search 27))))
  (is (= true (boolean (sut/perfect-cube-binary-search 64))))
  (is (= false (boolean (sut/perfect-cube-binary-search 4))))
  (is (thrown? Exception (sut/perfect-cube-binary-search "a")))
  (is (thrown? Exception (sut/perfect-cube-binary-search 0.1))))
