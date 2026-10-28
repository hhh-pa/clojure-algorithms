(ns clojure-algorithms.maths.autocorrelation-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.autocorrelation :as sut]))

(deftest test-fn-autocorrelation-examples
  (is (approx 0.4 (sut/autocorrelation [1 2 3 4 5] 1)))
  (is (approx 1.0 (sut/autocorrelation [1 2 3 4 5] 0)))
  (is (thrown? Exception (sut/autocorrelation [1 2 3] 5))))

(deftest test-fn-autocorrelation-errors
  (is (= :ValueError (try (sut/autocorrelation [1 2 3] 5)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e))))))
  (is (= :ValueError (try (sut/autocorrelation [5 5 5] 1)
                          nil
                          (catch clojure.lang.ExceptionInfo e
                            (:type (ex-data e)))))))
