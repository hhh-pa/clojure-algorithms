(ns clojure-algorithms.maths.persistence-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.persistence :as sut]))

(deftest test-fn-multiplicative-persistence-examples
  (is (approx 2 (sut/multiplicative-persistence 217)))
  (is (thrown? Exception (sut/multiplicative-persistence -1)))
  (is (thrown? Exception (sut/multiplicative-persistence "long number"))))

(deftest test-fn-additive-persistence-examples
  (is (approx 3 (sut/additive-persistence 199)))
  (is (thrown? Exception (sut/additive-persistence -1)))
  (is (thrown? Exception (sut/additive-persistence "long number"))))
