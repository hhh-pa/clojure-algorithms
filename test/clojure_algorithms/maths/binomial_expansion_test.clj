(ns clojure-algorithms.maths.binomial-expansion-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.binomial-expansion :as sut]))

(deftest test-fn-binomial-expansion-examples
  (is (approx 25 (sut/binomial-expansion 2 3 2)))
  (is (approx 884736 (sut/binomial-expansion 100 -4 3)))
  (is (approx 0.0625 (sut/binomial-expansion 2 2 -2)))
  (is (approx 0 (sut/binomial-expansion 0 0 3)))
  (is (thrown? Exception (sut/binomial-expansion -2 2 -1))))

(deftest test-fn-binomial-expansion-extra
  (is (= :ZeroDivisionError (try (sut/binomial-expansion -2 2 -1) nil
                                 (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
  (is (approx 1 (sut/binomial-expansion 5 7 0)))
  (is (approx 8 (sut/binomial-expansion 1 1 3))))
