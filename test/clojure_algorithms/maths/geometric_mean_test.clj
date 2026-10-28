(ns clojure-algorithms.maths.geometric-mean-test
  (:require [clojure-algorithms.test-utils :refer [approx]]
            [clojure.test :refer [deftest is]]
            [clojure-algorithms.maths.geometric-mean :as sut]))

(deftest test-fn-compute-geometric-mean-examples
  (is (approx 4.0 (sut/compute-geometric-mean 2 8)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/compute-geometric-mean "a" 4)))
  (is (approx 25.0 (sut/compute-geometric-mean 5 125)))
  (is (approx 0.0 (sut/compute-geometric-mean 1 0)))
  (is (approx 5.0 (sut/compute-geometric-mean 1 5 25 5)))
  (is (thrown? clojure.lang.ExceptionInfo (sut/compute-geometric-mean 2 -2)))
  (is (approx -5.0 (sut/compute-geometric-mean -5 25 1))))
