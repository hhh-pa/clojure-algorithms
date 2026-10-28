(ns clojure-algorithms.maths.binomial-expansion-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.binomial-expansion :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/binomial-expansion)))
