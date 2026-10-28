(ns clojure-algorithms.maths.sum-of-arithmetic-series-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.sum-of-arithmetic-series :as sut]))

(deftest test-fn-sum-of-ap-series-examples
  (is (approx 15 (sut/sum-of-ap-series 1 1 5)))
  (is (approx 26 (sut/sum-of-ap-series 2 3 4)))
  (is (approx 15 (sut/sum-of-ap-series 5 0 3)))
  (is (approx 1 (sut/sum-of-ap-series 1 2 1)))
  (is (approx -5 (sut/sum-of-ap-series 1 -1 5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/sum-of-ap-series 1 1 -5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/sum-of-ap-series 1 1 0))))
