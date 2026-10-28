(ns clojure-algorithms.maths.remove-digit-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.remove-digit :as sut]))

(deftest test-fn-remove-digit-examples
  (is (thrown? Exception (sut/remove-digit "2222222")))
  (is (thrown? Exception (sut/remove-digit "string input"))))
