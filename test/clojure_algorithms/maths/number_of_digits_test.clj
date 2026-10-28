(ns clojure-algorithms.maths.number-of-digits-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.number-of-digits :as sut]))

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

(deftest test-fn-num-digits-examples
  (is (approx 5 (sut/num-digits 12345)))
  (is (approx 3 (sut/num-digits 123)))
  (is (approx 1 (sut/num-digits 0)))
  (is (approx 1 (sut/num-digits -1)))
  (is (approx 6 (sut/num-digits -123456)))
  (is (thrown? Exception (sut/num-digits "123"))))

(deftest test-fn-num-digits-fast-examples
  (is (approx 5 (sut/num-digits-fast 12345)))
  (is (approx 3 (sut/num-digits-fast 123)))
  (is (approx 4 (sut/num-digits-fast 1000)))
  (is (approx 16 (sut/num-digits-fast 1000000000000000)))
  (is (approx 1 (sut/num-digits-fast 0)))
  (is (approx 1 (sut/num-digits-fast -1)))
  (is (approx 6 (sut/num-digits-fast -123456)))
  (is (thrown? Exception (sut/num-digits-fast "123"))))

(deftest test-fn-num-digits-faster-examples
  (is (approx 5 (sut/num-digits-faster 12345)))
  (is (approx 3 (sut/num-digits-faster 123)))
  (is (approx 1 (sut/num-digits-faster 0)))
  (is (approx 1 (sut/num-digits-faster -1)))
  (is (approx 6 (sut/num-digits-faster -123456)))
  (is (thrown? Exception (sut/num-digits-faster "123"))))
