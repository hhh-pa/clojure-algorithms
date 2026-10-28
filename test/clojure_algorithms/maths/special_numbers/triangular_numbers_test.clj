(ns clojure-algorithms.maths.special-numbers.triangular-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.triangular-numbers :as sut]))

(deftest test-fn-triangular-number-examples
  (is (approx 1 (sut/triangular-number 1)))
  (is (approx 6 (sut/triangular-number 3)))
  (is (thrown? Exception (sut/triangular-number -1))))

(deftest test-triangular-sequence
  (is (approx [0 1 3 6 10 15 21] (mapv sut/triangular-number (range 7)))))

(deftest test-error-type
  (is (= :ValueError
         (try (sut/triangular-number -1)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
