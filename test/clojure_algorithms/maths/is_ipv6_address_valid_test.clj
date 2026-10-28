(ns clojure-algorithms.maths.is-ipv6-address-valid-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.is-ipv6-address-valid :as sut]))

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

(deftest test-module-examples
  (is (= true (boolean (sut/is-ipv6-address-valid "::"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ""))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:::8a2e:370:7334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:370:7334:abcd"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:0370:g334"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "2001:db8:85a3:0:0:8a2e:0370:"))))
  (is (= false (boolean (sut/is-ipv6-address-valid ":2001:db8::1"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "a::b:c:d:e:f:g:h"))))
  (is (= false (boolean (sut/is-ipv6-address-valid "1::1:1:1:1:1:1:1")))))

(deftest test-fn-is-valid-group-examples
  (is (= false (boolean (sut/-is-valid-group "12345"))))
  (is (= false (boolean (sut/-is-valid-group "g")))))
