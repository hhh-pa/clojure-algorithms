(ns clojure-algorithms.maths.next-prime-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.next-prime-number :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-is-prime-examples
  (is (= true (boolean (sut/is-prime 2))))
  (is (= false (boolean (sut/is-prime 15))))
  (is (= true (boolean (sut/is-prime 19))))
  (is (= false (boolean (sut/is-prime 1))))
  (is (= false (boolean (sut/is-prime -7)))))

(deftest test-fn-next-prime-examples
  (is (approx 3 (sut/next-prime 2)))
  (is (approx 11 (sut/next-prime 7)))
  (is (approx 17 (sut/next-prime 14)))
  (is (approx 2 (sut/next-prime 0)))
  (is (approx 2 (sut/next-prime -10))))

(deftest test-next-prime-errors
  (is (= :ValueError (err-type #(sut/next-prime 2.5))))
  (is (= :ValueError (err-type #(sut/next-prime "7"))))
  (is (thrown? Exception (sut/next-prime 2.5))))

(deftest test-next-prime-extra
  (is (= 101 (sut/next-prime 100)))
  (is (true? (sut/is-prime (sut/next-prime 1000)))))
