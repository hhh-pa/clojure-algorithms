(ns clojure-algorithms.maths.numerical-analysis.gauss-seidel-method-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.numerical-analysis.gauss-seidel-method :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/gauss-seidel)))
