(ns clojure-algorithms.maths.is-ip-v4-address-valid-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-ip-v4-address-valid :as sut]))

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

(deftest test-fn-is-ip-v4-address-valid-examples
  (is (= true (boolean (sut/is-ip-v4-address-valid "192.168.0.23"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "192.256.15.8"))))
  (is (= true (boolean (sut/is-ip-v4-address-valid "172.100.0.8"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "255.256.0.256"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.33333333.4"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.-3.4"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3.4.5"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.A.4"))))
  (is (= true (boolean (sut/is-ip-v4-address-valid "0.0.0.0"))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3."))))
  (is (= false (boolean (sut/is-ip-v4-address-valid "1.2.3.05")))))
