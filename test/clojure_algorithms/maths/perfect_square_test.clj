(ns clojure-algorithms.maths.perfect-square-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.perfect-square :as sut]))

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

(deftest test-fn-perfect-square-examples
  (is (= true (boolean (sut/perfect-square 9))))
  (is (= true (boolean (sut/perfect-square 16))))
  (is (= true (boolean (sut/perfect-square 1))))
  (is (= true (boolean (sut/perfect-square 0))))
  (is (= false (boolean (sut/perfect-square 10)))))

(deftest test-fn-perfect-square-binary-search-examples
  (is (= true (boolean (sut/perfect-square-binary-search 9))))
  (is (= true (boolean (sut/perfect-square-binary-search 16))))
  (is (= true (boolean (sut/perfect-square-binary-search 1))))
  (is (= true (boolean (sut/perfect-square-binary-search 0))))
  (is (= false (boolean (sut/perfect-square-binary-search 10))))
  (is (= false (boolean (sut/perfect-square-binary-search -1))))
  (is (= false (boolean (sut/perfect-square-binary-search 1.1))))
  (is (thrown? Exception (sut/perfect-square-binary-search "a")))
  (is (thrown? Exception (sut/perfect-square-binary-search []))))
