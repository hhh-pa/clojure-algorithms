(ns clojure-algorithms.maths.allocation-number-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.allocation-number :as sut]))

(deftest test-fn-allocation-num-examples
  (is (approx ["1-4161" "4162-8322" "8323-12483" "12484-16647"] (sut/allocation-num 16647 4)))
  (is (approx ["1-10000" "10001-20000" "20001-30000" "30001-40000" "40001-50000"] (sut/allocation-num 50000 5)))
  (is (thrown? Exception (sut/allocation-num 888 999)))
  (is (thrown? Exception (sut/allocation-num 888 -4))))
