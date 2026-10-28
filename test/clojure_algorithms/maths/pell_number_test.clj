(ns clojure-algorithms.maths.pell-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.pell-number :as sut]))

(deftest test-fn-pell-number-iterative-examples
  (is (approx 0 (sut/pell-number-iterative 0)))
  (is (approx 1 (sut/pell-number-iterative 1)))
  (is (approx 13860 (sut/pell-number-iterative 12)))
  (is (thrown? Exception (sut/pell-number-iterative "1")))
  (is (thrown? Exception (sut/pell-number-iterative -1))))

(deftest test-fn-pell-number-recursive-examples
  (is (approx 0 (sut/pell-number-recursive 0)))
  (is (approx 1 (sut/pell-number-recursive 1)))
  (is (approx 13860 (sut/pell-number-recursive 12)))
  (is (thrown? Exception (sut/pell-number-recursive "1")))
  (is (thrown? Exception (sut/pell-number-recursive -1))))
