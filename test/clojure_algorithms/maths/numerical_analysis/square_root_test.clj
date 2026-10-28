(ns clojure-algorithms.maths.numerical-analysis.square-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.numerical-analysis.square-root :as sut]))

(deftest test-fn-square-root-iterative-examples
  (is (thrown? Exception (sut/square-root-iterative -1)))
  (is (approx 2.0 (sut/square-root-iterative 4)))
  (is (approx 1.788854381999832 (sut/square-root-iterative 3.2)))
  (is (approx 11.832159566199232 (sut/square-root-iterative 140))))
