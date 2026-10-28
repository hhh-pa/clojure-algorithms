(ns clojure-algorithms.maths.special-numbers.hamming-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.hamming-numbers :as sut]))

(deftest test-fn-hamming-examples
  (is (thrown? Exception (sut/hamming -5)))
  (is (approx [1 2 3 4 5] (sut/hamming 5)))
  (is (approx [1 2 3 4 5 6 8 9 10 12] (sut/hamming 10)))
  (is (approx [1 2 3 4 5 6 8 9 10 12 15 16 18 20 24] (sut/hamming 15))))

(deftest test-hamming-single
  (is (approx [1] (sut/hamming 1))))

(deftest test-error-type
  (is (= :ValueError
         (try (sut/hamming -5)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
