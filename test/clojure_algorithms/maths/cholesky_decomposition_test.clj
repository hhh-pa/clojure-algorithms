(ns clojure-algorithms.maths.cholesky-decomposition-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-algorithms.maths.cholesky-decomposition :as sut]))

(deftest test-vars-exist
  (is (some? #'sut/cholesky-decomposition))
  (is (some? #'sut/solve-cholesky)))
