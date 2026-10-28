(ns clojure-algorithms.maths.double-factorial-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.double-factorial :as sut]))

(deftest test-fn-double-factorial-recursive-examples
  (is (thrown? Exception (sut/double-factorial-recursive 0.1)))
  (is (thrown? Exception (sut/double-factorial-recursive -1))))

(deftest test-fn-double-factorial-iterative-examples
  (is (thrown? Exception (sut/double-factorial-iterative 0.1)))
  (is (thrown? Exception (sut/double-factorial-iterative -1))))
