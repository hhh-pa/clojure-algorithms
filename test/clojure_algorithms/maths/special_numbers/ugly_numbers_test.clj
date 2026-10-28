(ns clojure-algorithms.maths.special-numbers.ugly-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.ugly-numbers :as sut]))

(deftest test-fn-ugly-numbers-examples
  (is (approx 1536 (sut/ugly-numbers 100)))
  (is (approx 1 (sut/ugly-numbers 0)))
  (is (approx 36 (sut/ugly-numbers 20)))
  (is (approx 1 (sut/ugly-numbers -5)))
  (is (thrown? Exception (sut/ugly-numbers -5.5))))

(deftest test-ugly-sequence-start
  (is (approx [1 2 3 4 5 6 8 9 10 12 15] (mapv sut/ugly-numbers (range 1 12)))))

(deftest test-error-type
  (is (= :TypeError
         (try (sut/ugly-numbers -5.5)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
