(ns clojure-algorithms.maths.perfect-number-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.perfect-number :as sut]))

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

(deftest test-fn-perfect-examples
  (is (= true (boolean (sut/perfect 6))))
  (is (= true (boolean (sut/perfect 28))))
  (is (= true (boolean (sut/perfect 496))))
  (is (= true (boolean (sut/perfect 8128))))
  (is (= true (boolean (sut/perfect 33550336))))
  (is (= false (boolean (sut/perfect 12))))
  (is (= false (boolean (sut/perfect 27))))
  (is (= false (boolean (sut/perfect 29))))
  (is (= false (boolean (sut/perfect 100))))
  (is (= false (boolean (sut/perfect 1))))
  (is (= false (boolean (sut/perfect 2))))
  (is (= false (boolean (sut/perfect 0))))
  (is (= false (boolean (sut/perfect -1))))
  (is (= false (boolean (sut/perfect -6))))
  (is (= false (boolean (sut/perfect 5))))
  (is (= false (boolean (sut/perfect 7))))
  (is (= false (boolean (sut/perfect 27))))
  (is (= false (boolean (sut/perfect 29))))
  (is (= false (boolean (sut/perfect 495))))
  (is (= false (boolean (sut/perfect 497))))
  (is (= false (boolean (sut/perfect 33550335))))
  (is (= false (boolean (sut/perfect 33550337))))
  (is (thrown? Exception (sut/perfect 12.34)))
  (is (thrown? Exception (sut/perfect "123")))
  (is (thrown? Exception (sut/perfect "Hello")))
  (is (thrown? Exception (sut/perfect [6])))
  (is (thrown? Exception (sut/perfect nil))))

(deftest test-fn-perfect-optimized-examples
  (is (= true (boolean (sut/perfect-optimized 6))))
  (is (= true (boolean (sut/perfect-optimized 28))))
  (is (= true (boolean (sut/perfect-optimized 496))))
  (is (= false (boolean (sut/perfect-optimized 12))))
  (is (= false (boolean (sut/perfect-optimized 1))))
  (is (= false (boolean (sut/perfect-optimized 0))))
  (is (= false (boolean (sut/perfect-optimized -1)))))

(deftest test-fn-find-perfect-numbers-examples
  (is (approx [6] (sut/find-perfect-numbers 10)))
  (is (approx [6 28] (sut/find-perfect-numbers 30)))
  (is (approx [6 28 496] (sut/find-perfect-numbers 500)))
  (is (approx [] (sut/find-perfect-numbers 0)))
  (is (approx [] (sut/find-perfect-numbers 1))))

(deftest test-fn-get-divisors-examples
  (is (approx [1 2 3] (sut/get-divisors 6)))
  (is (approx [1 2 4 7 14] (sut/get-divisors 28)))
  (is (approx [1 2 3 4 6] (sut/get-divisors 12)))
  (is (approx [] (sut/get-divisors 1)))
  (is (approx [1] (sut/get-divisors 7))))
