(ns clojure-algorithms.maths.integer-square-root-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.integer-square-root :as sut]))

(deftest test-fn-integer-square-root-examples
  (is (approx 25 (sut/integer-square-root 625)))
  (is (approx 46340 (sut/integer-square-root 2147483647)))
  (is (thrown? Exception (sut/integer-square-root -1)))
  (is (thrown? Exception (sut/integer-square-root 1.5)))
  (is (thrown? Exception (sut/integer-square-root "0"))))
