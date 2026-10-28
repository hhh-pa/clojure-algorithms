(ns clojure-algorithms.maths.sylvester-sequence-test
  (:require [clojure-algorithms.test-utils :refer [approx]] [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure-algorithms.maths.sylvester-sequence :as sut]))

(deftest test-fn-sylvester-examples
  (is (approx 113423713055421844361000443N (sut/sylvester 8)))
  (is (thrown? Exception (sut/sylvester -1))))
