(ns clojure-algorithms.maths.sylvester-sequence-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.sylvester-sequence :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-sylvester-examples
  (is (approx 113423713055421844361000443N (sut/sylvester 8)))
  (is (thrown? Exception (sut/sylvester -1)))
  (is (thrown? AssertionError (sut/sylvester 8.0)))
  (is (= :ValueError (err-type #(sut/sylvester -1))))
  (is (= :ValueError (err-type #(sut/sylvester 0)))))

(deftest test-sylvester-extra
  (is (= 2 (sut/sylvester 1)))
  (is (= 3 (sut/sylvester 2)))
  (is (= 7 (sut/sylvester 3)))
  (is (= 43 (sut/sylvester 4)))
  (is (= [2 3 7 43 1807] (mapv sut/sylvester (range 1 6)))))
