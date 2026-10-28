(ns clojure-algorithms.maths.special-numbers.catalan-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.catalan-number :as sut]))

(deftest test-fn-catalan-examples
  (is (approx 14 (sut/catalan 5)))
  (is (thrown? Exception (sut/catalan 0)))
  (is (thrown? Exception (sut/catalan -1)))
  (is (thrown? Exception (sut/catalan 5.0))))

(deftest test-catalan-sequence
  (is (approx [1 1 2 5 14 42 132] (mapv sut/catalan (range 1 8)))))

(deftest test-error-types
  (is (= :ValueError
         (try (sut/catalan 0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e))))))
  (is (= :TypeError
         (try (sut/catalan 5.0)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
