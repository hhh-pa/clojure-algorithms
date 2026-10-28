(ns clojure-algorithms.maths.numerical-analysis.nth-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.nth-root :as sut]))

(deftest test-fn-nth-root-examples
  (is (approx 0.0 (sut/nth-root 0 2 0.1)))
  (is (approx 0.0 (sut/nth-root 0.0 5)))
  (is (thrown? Exception (sut/nth-root "invalid input" 3 0.0001)))
  (is (thrown? Exception (sut/nth-root 4 0.5 0.0001)))
  (is (thrown? Exception (sut/nth-root 16 -4 0.001)))
  (is (thrown? Exception (sut/nth-root 4 2 "0.000001")))
  (is (thrown? Exception (sut/nth-root 9 2 -0.01)))
  (is (thrown? Exception (sut/nth-root -256 4 0.0001))))
