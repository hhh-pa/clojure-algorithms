(ns clojure-algorithms.maths.aliquot-sum-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.aliquot-sum :as sut]))

(deftest test-fn-aliquot-sum-examples
  (is (approx 9 (sut/aliquot-sum 15)))
  (is (approx 6 (sut/aliquot-sum 6)))
  (is (thrown? Exception (sut/aliquot-sum -1)))
  (is (thrown? Exception (sut/aliquot-sum 0)))
  (is (thrown? Exception (sut/aliquot-sum 1.6)))
  (is (approx 16 (sut/aliquot-sum 12)))
  (is (approx 0 (sut/aliquot-sum 1)))
  (is (approx 1 (sut/aliquot-sum 19))))
