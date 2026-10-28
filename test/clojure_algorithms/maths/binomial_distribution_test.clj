(ns clojure-algorithms.maths.binomial-distribution-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.binomial-distribution :as sut]))

(deftest test-fn-binomial-distribution-examples
  (is (approx 0.30870000000000003 (sut/binomial-distribution 3 5 0.7)))
  (is (approx 0.375 (sut/binomial-distribution 2 4 0.5))))

(deftest test-fn-binomial-distribution-errors
  (is (thrown? Exception (sut/binomial-distribution 6 5 0.5)))
  (is (thrown? Exception (sut/binomial-distribution -1 5 0.5)))
  (is (thrown? Exception (sut/binomial-distribution 2.5 4 0.5)))
  (is (thrown? Exception (sut/binomial-distribution 2 4 1.5)))
  (is (thrown? Exception (sut/binomial-distribution 2 4 0)))
  (is (= :ValueError (try (sut/binomial-distribution 6 5 0.5)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :ValueError (try (sut/binomial-distribution 2 4 2)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))
