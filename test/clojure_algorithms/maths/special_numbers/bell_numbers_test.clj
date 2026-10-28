(ns clojure-algorithms.maths.special-numbers.bell-numbers-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.special-numbers.bell-numbers :as sut]))

(deftest test-fn-bell-numbers-examples
  (is (thrown? Exception (sut/bell-numbers -2)))
  (is (approx [1] (sut/bell-numbers 0)))
  (is (approx [1 1] (sut/bell-numbers 1)))
  (is (approx [1 1 2 5 15 52] (sut/bell-numbers 5))))

(deftest test-fn-binomial-coefficient-examples
  (is (approx 10 (sut/-binomial-coefficient 5 2)))
  (is (approx 20 (sut/-binomial-coefficient 6 3))))

(deftest test-error-type
  (is (= :ValueError
         (try (sut/bell-numbers -2)
              nil
              (catch clojure.lang.ExceptionInfo e
                (:type (ex-data e)))))))
