(ns clojure-algorithms.maths.combinations-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.combinations :as sut]))

(defn- err-type [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))

(deftest test-fn-combinations-examples
  (is (approx 252 (sut/combinations 10 5)))
  (is (approx 20 (sut/combinations 6 3)))
  (is (approx 15504 (sut/combinations 20 5)))
  (is (approx 2598960 (sut/combinations 52 5)))
  (is (approx 1 (sut/combinations 0 0)))
  (is (thrown? Exception (sut/combinations -4 -5)))
  (is (= :ValueError (err-type #(sut/combinations -4 -5))))
  (is (= :ValueError (err-type #(sut/combinations 3 5))))
  (is (= :ValueError (err-type #(sut/combinations 5 -1)))))

(deftest test-combinations-extra
  (is (= 1 (sut/combinations 7 0)))
  (is (= 1 (sut/combinations 7 7)))
  (is (= 40 (sut/combinations 40 1)))
  (is (= 100891344545564193334812497256 (sut/combinations 100 50))))
